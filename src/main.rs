use clap::*;
use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use crate::compiler::CompileOption;

mod compiler;

#[derive(Parser)]
#[command(about)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        file_path: String,

        #[arg(short, long)]
        outdir: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build { file_path, outdir } => {
            let input_code = {
                let mut tmp = String::with_capacity(256);
                let stdin = File::open(file_path).expect("msg");
                let mut stdin = std::io::BufReader::new(stdin);

                stdin.read_to_string(&mut tmp);
                tmp
            };

            let output_code = compiler::Compiler::compile_static(
                Path::new(file_path)
                    .file_name()
                    .expect("msg")
                    .to_str()
                    .expect("msg")
                    .to_string(),
                input_code,
                CompileOption::default(),
            );

            let stdout = File::open(outdir).expect("msg");
            let mut stdout = std::io::BufWriter::new(stdout);
            stdout.write_all(output_code.as_bytes());
        }
    }
}
