use chocolatier::xcode::Target;
use chocolatier::{ast, generator};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "generate",
    about = "Generate all bindings from specified file."
)]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Target
    #[structopt(long, possible_values = &Target::variants(), default_value = Target::MacOsX86_64.triple())]
    target: Target,
}

fn main() {
    let opt = Opt::from_args();

    let content = std::fs::read_to_string(opt.input).unwrap();
    let items = ast::ast_from_str(opt.target, &content).unwrap();
    let mut generator = generator::Generator::new(items);
    generator.generate();
}
