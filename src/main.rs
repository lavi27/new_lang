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

        #[arg(long)]
        release: bool,
        #[arg(long)]
        dev: bool,
    },
    Run {
        file_path: String,

        #[arg(short, long)]
        outdir: String,

        #[arg(long)]
        release: bool,
        #[arg(long)]
        dev: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build {
            file_path,
            outdir,
            transfile,
            release,
            dev,
        } => {
            let profile = if *release {
                "release".to_string()
            } else if *dev {
                "dev".to_string()
            } else {
                "release".to_string()
            };

            compiler::Compiler::compile_static(
                file_path.clone(),
                outdir.clone(),
                CompileOption {
                    transfile: *transfile,
                    run: false,
                    profile,
                },
            );
        }
        Commands::Run {
            file_path,
            outdir,
            release,
            dev,
        } => {
            let profile = if *release {
                "release".to_string()
            } else if *dev {
                "dev".to_string()
            } else {
                "release".to_string()
            };

            compiler::Compiler::compile_static(
                file_path.clone(),
                outdir.clone(),
                CompileOption {
                    transfile: false,
                    run: true,
                    profile,
                },
            );
        }
    }
}
