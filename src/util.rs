use std::rc::Rc;
use ascent::{ascent, Lattice};
use lexpr::{Value, parse::Error, cons};
use std::fs;
use std::env;
use std::io::Read;
use std::process::{Command, ExitStatus};
use std::io::{self, Write};
use crate::ast;
use std::collections::HashMap;
use std::any::Any;
use crate::x86::mach;


pub fn read_file(filename:&str) -> String {
    let mut file = fs::File::open(filename).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}


// Got from chatgpt
pub fn test_command(command: &str, args: &[&str]) -> io::Result<ExitStatus> {
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
pub fn test_load_machfile(){

    test_command("sh", &["test/test_printMach.sh"]);
    let data = read_file("sample.mach");
    test_command("rm", &["-rf", "sample.mach", "test/a.out", "a.out"]);   
    let program = ast::Program::from(data);

}