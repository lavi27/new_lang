use clap::*;
use std::{fs::File, io::Read};

mod compiler;

#[derive(Parser)]
#[command(about)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(subcommand)]
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
            let stdin = File::open(file_path).expect("msg");
            let mut stdin = std::io::BufReader::new(stdin);
            let input_code = String::with_capacity(256);
            stdin.read_to_string(&mut input_code);

            let stdout = File::open(outdir).expect("msg");
            let mut stdout = std::io::BufWriter::new(stdout);

            let output_code = compiler::Compiler::compile(input_code);

            writeln!(stdout, output_code).unwrap();
        }
    }
}
