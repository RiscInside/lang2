use bumpalo::Bump;
use clap::Parser;
use lang2_ast::builder::Builder;
use lang2_parser::parse;
use owo_colors::OwoColorize;
use serde::Deserialize;
use std::{collections::VecDeque, io::Write, path::PathBuf};
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
    ASTParsesTo { source: String, expected: String },
}

#[derive(Deserialize)]
struct ListedTasks {
    tasks: Vec<ListedTask>,
}

#[derive(Clone)]
enum Task {
    Include { path: PathBuf },
    ASTParsesTo { path: PathBuf, expected: PathBuf },
}

enum Failure {
    ASTParsesToFailure {
        path: PathBuf,
        expected: PathBuf,
        actual: Option<PathBuf>,
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
                        ListedTask::ASTParsesTo { source, expected } => {
                            self.tasks.push_back(Task::ASTParsesTo {
                                path: parent.join(source),
                                expected: parent.join(expected),
                            });
                        }
                    }
                }
            }
            Task::ASTParsesTo { path, expected } => {
                self.overall += 1;
                bump.reset();
                println!(
                    "Running {} test for {}",
                    "ASTParsesTo".bold(),
                    path.display().underline()
                );
                let source = std::fs::read_to_string(&path).unwrap();
                let expected_str = if !expected.exists() && self.bless {
                    "".to_owned()
                } else {
                    std::fs::read_to_string(&expected).unwrap()
                };
                let builder = Builder::new(&bump);
                let Ok(ast) = parse(&source, builder) else {
                    println!(
                        "> {} {} for {} - {}",
                        "ASTParsesTo".bold(),
                        "failed".bright_red().bold(),
                        path.display().underline(),
                        "parser returned with an error".bright_red().bold(),
                    );
                    self.failed.push(Failure::ASTParsesToFailure {
                        path,
                        expected,
                        actual: None,
                    });
                    return;
                };

                let mut actual_str = serde_json::to_string_pretty(&ast.top).unwrap();
                if !actual_str.ends_with("\n") {
                    actual_str.push('\n');
                }

                if expected_str != actual_str {
                    if self.bless {
                        println!(
                            "> {} {} for {}, {} {} (bless mode enabled)",
                            "ASTParsesTo".bold(),
                            "failed".bright_blue().bold(),
                            path.display().underline(),
                            "overwriting".bright_blue().bold(),
                            expected.display().underline()
                        );
                        std::fs::write(&expected, &actual_str).unwrap();
                        self.blessed += 1;
                    } else {
                        let mut out = NamedTempFile::new().unwrap();
                        out.write_all(actual_str.as_bytes()).unwrap();
                        println!(
                            "> {} {} for {}, writing actual output to {}",
                            "ASTParsesTo".bold(),
                            "failed".bright_red().bold(),
                            path.display().underline(),
                            out.path().display().underline()
                        );
                        self.failed.push(Failure::ASTParsesToFailure {
                            path,
                            expected,
                            actual: Some(out.path().to_path_buf()),
                        });
                        out.keep().unwrap();
                        return;
                    }
                } else {
                    self.passed += 1;
                }
            }
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
                Failure::ASTParsesToFailure {
                    path,
                    expected,
                    actual,
                } => {
                    if let Some(actual) = actual {
                        println!(
                            ">>> {} (source at {}, expected output at {}, actual output saved to {})",
                            "ASTParsesTo".bold(),
                            path.display().underline(),
                            expected.display().underline(),
                            actual.display().underline()
                        );
                        println!(
                            ">>>>> See `diff --color {} {}` for diff",
                            expected.display(),
                            actual.display(),
                        )
                    } else {
                        println!(
                            ">>> {} (source at {}, expected output at {}, {})",
                            "ASTParsesTo".bold(),
                            path.display().underline(),
                            expected.display().underline(),
                            "failed to parse".bright_red().bold(),
                        );
                    }
                    println!(
                        ">>>>> See `cargo run --bin lang2c -- ast {}` for more info",
                        path.display()
                    )
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
