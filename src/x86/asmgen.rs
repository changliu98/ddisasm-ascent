use ascent::ascent;

use crate::util::Either;
use crate::ast;
use crate::x86::reg;
use crate::x86::op;
use crate::x86::mach;
use crate::util;

pub type Label = usize;

ascent! {
    pub struct Mach2Asm;

    // https://github.com/AbsInt/CompCert/blob/4f638e8fc778109e92cee51f508852c2e22a5352/x86/Asmgen.v#L693

    relation instr(mach::Function, mach::Instruction, bool);
    
}

#[test]
fn translate_mach_asm() {
    util::test_command("sh", &["test_scripts/test_printMach.sh"]);
    let data = util::read_file("sample.mach");
    // test_command("rm", &["-rf", "sample.mach", "test_scripts/a.out"]);   
    let mut program = ast::Program::from(data);
    let mut functions = program.prog_defs;
    let mut instr_vec = Vec::new();
    for (ident, globedef) in &mut functions {
        // Identifier, GlobDef<F, V>
        let function = match globedef {
            ast::GlobDef::GFun(function) => function,
            // Also need to fix in printmach.ml
            _ => continue,
        };
        let instrs = &function.fn_code;
        for instr in instrs {
            instr_vec.push((function.clone(), instr.clone(), true));
        }
    }
    let mut prog = Mach2Asm::default();

    prog.instr = instr_vec;
    prog.run();

}