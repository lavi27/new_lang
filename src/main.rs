#![deny(deprecated)]

use clap::*;

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
        #[arg(short, long)]
        transfile: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build { file_path, outdir, transfile } => {
            compiler::Compiler::compile_static(
                file_path.clone(),
                outdir.clone(),
                CompileOption {
                    transfile: transfile.clone(),
                },
            );
        }
    }
}
