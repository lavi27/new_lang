use clap::Parser;

#![feature(pattern)]
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
    }
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build {file_path, outdir} => {
            let stdin = File::open(file_path);
            let stdin = stdin.lock();
            let mut stdin = std::io::BufReader::new(stdin);
            let input_code = stdin.read();

            let stdout = File::open(outdir);
            let stdout = stdout.lock();
            let mut stdout = std::io::BufWriter::new(stdout);

            let output_code = Compiler::compile(input_code);

            writeln!(stdout, output_code).unwrap();
        }
    }
}
