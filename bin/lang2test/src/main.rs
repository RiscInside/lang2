use bumpalo::Bump;
use clap::Parser;
use lang2_ast::builder::Builder;
use lang2_misc::Errors;
use lang2_parser::parse;
use lang2_sema::preprocessing::preprocess;
use owo_colors::OwoColorize;
use serde::Deserialize;
use std::{
    collections::VecDeque,
    io::Write,
    path::{Path, PathBuf},
};
use tempfile::NamedTempFile;

#[derive(Parser)]
struct Args {
    /// Path to the manifest
    manifest_path: PathBuf,
    /// Bless mode
    #[arg(
        long,
        help = "Overwrite expected output with actual output for failing tests"
    )]
    bless: bool,
}

#[derive(Deserialize, Clone)]
#[serde(tag = "kind")]
enum ListedTask {
    Include { path: String },
    ParsesTo { source: String, expected: String },
    PreprocessesTo { source: String, expected: String },
}

#[derive(Deserialize)]
struct ListedTasks {
    tasks: Vec<ListedTask>,
}

#[derive(Clone, Copy)]
enum FunctionOfSource {
    Parse,
    Preprocess,
}

impl FunctionOfSource {
    fn compiler_subcommand(self) -> &'static str {
        match self {
            FunctionOfSource::Parse => "parse",
            FunctionOfSource::Preprocess => "preprocess",
        }
    }

    fn name(self) -> &'static str {
        match self {
            FunctionOfSource::Parse => "ParsesTo",
            FunctionOfSource::Preprocess => "PreprocessesTo",
        }
    }

    fn run(self, source: String, bump: &mut Bump) -> Result<String, Errors> {
        bump.reset();
        let mut errors = Errors::new();
        let ast = parse(&source, Builder::new(&bump), &mut errors);
        match self {
            FunctionOfSource::Parse => match ast {
                Some(ast) => Ok(serde_json::to_string_pretty(&ast.top).unwrap()),
                None => Err(errors),
            },
            FunctionOfSource::Preprocess => {
                let ast = match ast {
                    Some(ast) => ast,
                    None => return Err(errors),
                };
                Ok(serde_json::to_string_pretty(&preprocess(&ast, &bump)).unwrap())
            }
        }
    }
}

#[derive(Clone)]
enum Task {
    Include {
        path: PathBuf,
    },
    FromSource {
        function: FunctionOfSource,
        path: PathBuf,
        expected: PathBuf,
    },
}

enum Failure {
    Failed {
        function: FunctionOfSource,
        path: PathBuf,
    },
    Mismatch {
        function: FunctionOfSource,
        path: PathBuf,
        expected: PathBuf,
        actual: PathBuf,
    },
}

struct TestRunner {
    tasks: VecDeque<Task>,
    bless: bool,
    failed: Vec<Failure>,
    overall: usize,
    passed: usize,
    blessed: usize,
}

impl TestRunner {
    fn run_computation(
        &mut self,
        function: FunctionOfSource,
        source_path: &Path,
        expected_path: &Path,
        bump: &mut Bump,
    ) {
        self.overall += 1;
        bump.reset();
        println!(
            "Running {} test for {}",
            function.name().bold(),
            source_path.display().underline()
        );
        let source = std::fs::read_to_string(&source_path).unwrap();
        let expected_str = if !expected_path.exists() && self.bless {
            "".to_owned()
        } else {
            std::fs::read_to_string(&expected_path).unwrap()
        };
        let mut res = match function.run(source, bump) {
            Ok(res) => res,
            Err(errors) => {
                println!(
                    "> {} {} for {}",
                    function.name().bold(),
                    "failed to run".bright_red().bold(),
                    source_path.display().underline(),
                );
                println!("JSON error dump:");
                println!("{}", serde_json::to_string_pretty(&errors).unwrap());

                self.failed.push(Failure::Failed {
                    function: function,
                    path: source_path.to_path_buf(),
                });
                return;
            }
        };

        if !res.ends_with("\n") {
            res.push('\n');
        }

        if expected_str != res {
            if self.bless {
                println!(
                    "> {} {} for {}, {} {} (bless mode enabled)",
                    function.name().bold(),
                    "failed".bright_blue().bold(),
                    source_path.display().underline(),
                    "overwriting".bright_blue().bold(),
                    expected_path.display().underline()
                );
                std::fs::write(&expected_path, &res).unwrap();
                self.blessed += 1;
            } else {
                let mut out = NamedTempFile::new().unwrap();
                out.write_all(res.as_bytes()).unwrap();
                println!(
                    "> {} {} for {}, writing actual output to {}",
                    function.name().bold(),
                    "failed".bright_red().bold(),
                    source_path.display().underline(),
                    out.path().display().underline()
                );
                self.failed.push(Failure::Mismatch {
                    function,
                    path: source_path.to_path_buf(),
                    expected: expected_path.to_path_buf(),
                    actual: out.path().to_path_buf(),
                });
                out.keep().unwrap();
                return;
            }
        } else {
            self.passed += 1;
        }
    }

    fn run_task(&mut self, task: Task, bump: &mut Bump) {
        match task {
            Task::Include { path } => {
                let parent = path.parent().unwrap();
                let file = std::fs::read_to_string(&path).unwrap();
                let tasks: ListedTasks = serde_json::from_str(&file).unwrap();
                for task in tasks.tasks {
                    match task {
                        ListedTask::Include { path } => {
                            self.tasks.push_back(Task::Include {
                                path: parent.join(path),
                            });
                        }
                        ListedTask::ParsesTo { source, expected } => {
                            self.tasks.push_back(Task::FromSource {
                                function: FunctionOfSource::Parse,
                                path: parent.join(source),
                                expected: parent.join(expected),
                            });
                        }
                        ListedTask::PreprocessesTo { source, expected } => {
                            self.tasks.push_back(Task::FromSource {
                                function: FunctionOfSource::Preprocess,
                                path: parent.join(source),
                                expected: parent.join(expected),
                            })
                        }
                    }
                }
            }
            Task::FromSource {
                function,
                path,
                expected,
            } => self.run_computation(function, &path, &expected, bump),
        }
    }

    fn run(&mut self) {
        let mut bump = Bump::new();
        while let Some(task) = self.tasks.pop_front() {
            self.run_task(task, &mut bump)
        }
    }
}

pub fn main() {
    let args = Args::parse();
    let mut runner = TestRunner {
        tasks: VecDeque::new(),
        bless: args.bless,
        failed: vec![],
        overall: 0,
        passed: 0,
        blessed: 0,
    };
    runner.tasks.push_back(Task::Include {
        path: args.manifest_path,
    });

    runner.run();
    println!("SUMMARY");
    if runner.passed != 0 {
        println!(
            "> {} out of {} tests have {}",
            runner.passed,
            runner.overall,
            "passed".bright_green().bold()
        );
    }
    if runner.failed.len() != 0 {
        println!(
            "> {} tests {}",
            runner.failed.len(),
            "failed".bright_red().bold()
        );
        for failed in runner.failed {
            match failed {
                Failure::Failed { function, path, .. } => {
                    println!(
                        ">>> {} (source at {})",
                        function.name().bold(),
                        path.display().underline(),
                    );
                    println!(
                        ">>>>> See `cargo run --bin lang2c -- {} {}` for more info",
                        function.compiler_subcommand(),
                        path.display()
                    );
                }
                Failure::Mismatch {
                    function,
                    path,
                    expected,
                    actual,
                } => {
                    println!(
                        ">>> {} (source at {}, expected output at {}, actual output at {})",
                        function.name().bold(),
                        path.display().underline(),
                        expected.display().underline(),
                        actual.display().underline(),
                    );
                    println!(
                        ">>>>> See `cargo run --bin lang2c -- {} {}` for more info",
                        function.compiler_subcommand(),
                        path.display()
                    );
                    println!(
                        ">>>>> See `git diff --color {} {}` for the diff between expected and actual output",
                        expected.display(),
                        actual.display()
                    );
                }
            }
        }
    } else if runner.blessed != 0 {
        println!("> {} tests blessed", runner.blessed);
    }

    if runner.passed != runner.overall {
        std::process::exit(1)
    }
}
