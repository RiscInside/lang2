use bumpalo::Bump;
use clap::Parser;
use lang2_ast::builder::Builder;
use lang2_misc::Errors;
use lang2_parser::parse;
use lang2_sema::preprocessing::preprocess;

#[derive(Parser)]
struct Arguments {
    path: String,
}

fn main() {
    let mut args = std::env::args();
    let program_name = args.next().unwrap();
    let (Some(cmd), Some(path)) = (args.next(), args.next()) else {
        eprintln!("skill issue: incorrect usage, expected `{program_name} [cmd] [source]`");
        std::process::exit(1);
    };
    let _remaining: Vec<String> = args.collect();

    let bump = Bump::new();
    let mut errors = Errors::new();

    let file_source = if let Ok(source) = std::fs::read_to_string(&path) {
        source
    } else {
        eprintln!("skill issue: can't read file {}", &path);
        std::process::exit(1)
    };

    let ast = {
        let builder = Builder::new(&bump);
        parse(&*file_source, builder, &mut errors)
    };

    match cmd.as_str() {
        "parse" => match ast {
            Some(ast) => {
                println!("{}", serde_json::to_string_pretty(&ast.top).unwrap());
            }
            None => {
                eprintln!("skill issue, see error dump below:");
                println!("{}", serde_json::to_string_pretty(&errors).unwrap());
            }
        },
        "preprocess" => match ast {
            Some(ast) => {
                let preprocessed = preprocess(&ast, &bump);
                println!("{}", serde_json::to_string_pretty(&preprocessed).unwrap());
            }
            None => {
                eprintln!("skill issue, see error dump below:");
                println!("{}", serde_json::to_string_pretty(&errors).unwrap());
            }
        },
        unknown_cmd => {
            eprintln!("skill issue: command {unknown_cmd} not defined");
        }
    }
}
