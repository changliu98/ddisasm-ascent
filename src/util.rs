use std::rc::Rc;
use ascent::{ascent, Lattice};
use lexpr::{Value, parse::Error};
use std::fs;
use std::env;
use std::io::Read;
use std::process::{Command, ExitStatus};
use std::io::{self, Write};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

fn read_file(filename:&str) -> String {
    let mut file = fs::File::open(filename).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}


// Got from chatgpt
fn test_command(command: &str, args: &[&str]) -> io::Result<ExitStatus> {
    let mut cmd = Command::new(command);
    cmd.args(args);
    let result = cmd.status()?;
    if result.success() {
        println!("Command executed successfully!");
    } else {
        eprintln!("Command failed with status: {:?}", result);
    }
    Ok(result)
}




#[test]
fn test_printMach() -> Result<(), lexpr::parse::Error> {

    test_command("sh", &["test_scripts/test_printMach.sh"]);
    let data = read_file("sample.mach");
    let v = lexpr::from_str(&data)?;
    test_command("rm", &["-rf", "sample.mach"]);
    Ok(()) //Really?
    
}