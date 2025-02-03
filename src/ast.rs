use std::hash::{Hash, Hasher};
use lexpr::print;
use lexpr::{Value, parse::Error, cons};
use crate::x86::op::Ptrofs;
use crate::x86::mach::Function;
use crate::util::read_file;

pub type Ident = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Typ {
    Tint,
    Tfloat,
    Tlong,
    Tsingle,
    Tany32,
    Tany64,
}
impl From<String> for Typ {
    fn from(s: String) -> Self {
        match s.as_str() {
            "AST.typ.Tint" => Typ::Tint,
            "AST.typ.Tfloat" => Typ::Tfloat,
            "AST.typ.Tlong" => Typ::Tlong,
            "AST.typ.Tsingle" => Typ::Tsingle,
            "AST.typ.Tany32" => Typ::Tany32,
            "AST.typ.Tany64" => Typ::Tany64,
            _ => panic!("Invalid type at AST.typ {}", s),
        }
    }
}

// let string_of_xtype = function
// | Xbool -> "AST.xtype.Xbool"
// | Xint8signed -> "AST.xtype.Xint8signed"
// | Xint8unsigned -> "AST.xtype.Xint8unsigned"
// | Xint16signed -> "AST.xtype.Xint16signed"
// | Xint16unsigned -> "AST.xtype.Xint16unsigned"
// | Xint -> "AST.xtype.Xint"
// | Xfloat -> "AST.xtype.Xfloat"
// | Xlong -> "AST.xtype.Xlong"
// | Xsingle -> "AST.xtype.Xsingle"
// | Xptr -> "AST.xtype.Xptr"
// | Xany32 -> "AST.xtype.Xany32"
// | Xany64 -> "AST.xtype.Xany64"
// | Xvoid -> "AST.xtype.Xvoid"

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XType {
    Xbool,
    Xint8signed,
    Xint8unsigned,
    Xint16signed,
    Xint16unsigned,
    Xint,
    Xfloat,
    Xlong,
    Xsingle,
    Xptr,
    Xany32,
    Xany64,
    Xvoid,
}

impl From<String> for XType {
    fn from(s: String) -> Self {
        match s.as_str() {
            "AST.xtype.Xbool" => XType::Xbool,
            "AST.xtype.Xint8signed" => XType::Xint8signed,
            "AST.xtype.Xint8unsigned" => XType::Xint8unsigned,
            "AST.xtype.Xint16signed" => XType::Xint16signed,
            "AST.xtype.Xint16unsigned" => XType::Xint16unsigned,
            "AST.xtype.Xint" => XType::Xint,
            "AST.xtype.Xfloat" => XType::Xfloat,
            "AST.xtype.Xlong" => XType::Xlong,
            "AST.xtype.Xsingle" => XType::Xsingle,
            "AST.xtype.Xptr" => XType::Xptr,
            "AST.xtype.Xany32" => XType::Xany32,
            "AST.xtype.Xany64" => XType::Xany64,
            "AST.xtype.Xvoid" => XType::Xvoid,
            _ => panic!("Invalid type at AST.typ {}", s),
        }
    }
}


pub type Z = i64;
pub type Positive = i64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemoryChunk {
    MBool,
    MInt8Signed,
    MInt8Unsigned,
    MInt16Signed,
    MInt16Unsigned,
    MInt32,
    MInt64,
    MFloat32,
    MFloat64,
    MAny32,
    MAny64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub sig_args: Vec<XType>,
    pub sig_res: XType,
    pub sig_cc: CallConv,
}
impl Default for Signature {
    fn default() -> Self {
        Signature {
            sig_args: vec![],
            sig_res: XType::Xvoid,
            sig_cc: CallConv::default(),
        }
    }
}

// Record signature : Type := mksignature {
//     sig_args: list xtype;
//     sig_res: xtype;
//     sig_cc: calling_convention
//   }.

impl From<String> for Signature {
    fn from(s: String) -> Self {
        let parsed = lexpr::from_str(&s).expect("Failed to parse");
        match parsed {
            Value::Cons(cons) => {
                let head = cons.car();
                if *head != Value::Symbol("mksignature".into()) {
                    panic!("Failed to parse: {}, expected mksignature", head)
                }
                let cdr = cons.cdr();
                let mut args = cdr.list_iter().unwrap();
                let sig_args = args.next().unwrap();
                let sig_res = args.next().unwrap().to_string();
                let sig_cc = args.next().unwrap().to_string();
                Signature {
                    sig_args: sig_args.list_iter().unwrap().map(|s| XType::from(s.to_string())).collect(),
                    sig_res: XType::from(sig_res),
                    sig_cc: CallConv::default(),
                }
            }
            _ => panic!("Failed to parse Signature{}", s),
        }
    }
}

// #[test]
// fn test_load_signature(){
//     let data = read_file("sample.mach");
//     let s = Signature::from(data);
// }


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallConv {
    pub varargs: Option<i64>,
    pub unproto: bool,
    pub structured_ret: bool,
}
impl Default for CallConv {
    fn default() -> Self {
        CallConv {
            varargs: None,
            unproto: false,
            structured_ret: false,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternalFunction {
    EFExternal(String, Signature),
    EFBuiltin(String, Signature),
    EFRuntime(String, Signature),
    EFVLoad(MemoryChunk),
    EFVStore(MemoryChunk),
    EFMalloc,
    EFFree,
    EFMemcpy(Positive, Positive),
    EFAnnot(Positive, String, Vec<Typ>),
    EFAnnotVal(Positive, String, Typ),
    EFInlineAsm(String, Signature, Vec<String>),
    EFDebug(Positive, Ident, Vec<Typ>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinArg<T> {
    BA(T),
    BAInt(i64),
    BALong(i64),
    // BAFloat(f64),
    // BASingle(f32),
    BALoadStack(MemoryChunk, Ptrofs),
    BAAddrStack(Ptrofs),
    BALoadGlobal(MemoryChunk, Ident, Ptrofs),
    BAAddrGlobal(Ident, Ptrofs),
    BASplitLong(Box<BuiltinArg<T>>, Box<BuiltinArg<T>>),
    BAAddPtr(Box<BuiltinArg<T>>, Box<BuiltinArg<T>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinRes<T> {
    BR(T),
    BRNone,
    BRSplitLong(Box<BuiltinRes<T>>, Box<BuiltinRes<T>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitData {
    InitInt8(i64),
    InitInt16(i64),
    InitInt32(i64),
    InitInt64(i64),
    InitFloat32(f32),
    InitFloat64(f64),
    InitSpace(Z),
    InitAddrOf(Ident, Ptrofs),
}

// Implement eq for InitData
impl Eq for InitData {}
impl Hash for InitData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            InitData::InitInt8(i) => i.hash(state),
            InitData::InitInt16(i) => i.hash(state),
            InitData::InitInt32(i) => i.hash(state),
            InitData::InitInt64(i) => i.hash(state),
            InitData::InitFloat32(f) => f.to_bits().hash(state),
            InitData::InitFloat64(f) => f.to_bits().hash(state),
            InitData::InitSpace(z) => z.hash(state),
            InitData::InitAddrOf(i, p) => {
                i.hash(state);
                p.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobVar<V> {
    pub gvar_info: V,
    pub gvar_init: Vec<InitData>,
    pub gvar_readonly: bool,
    pub gvar_volatile: bool,
}


// Inductive globdef (F V: Type) : Type :=
//   | Gfun (f: F)
//   | Gvar (v: globvar V).

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GlobDef<F, V> {
    GFun(F),
    GVar(V),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program<F, V> {
    pub prog_defs: Vec<(Ident, GlobDef<F, V>)>,
    pub prog_public: Vec<Ident>,
    pub prog_main: Ident,
}
impl From<String> for Program<Function, GlobVar<MemoryChunk>> {
    fn from(s: String) -> Self {
        let parsed = lexpr::from_str(&s).expect("Failed to parse");
        match parsed {
            Value::Cons(cons) => {
                let head = cons.car();
                if *head != Value::Symbol("mkprogram".into()) {
                    panic!("Failed to parse: {}, expected mkprogram", head)
                }
                let args = cons.cdr();
                let mut args_iter = args.list_iter().unwrap();
                let defs = args_iter.next().unwrap();
                let public = args_iter.next().unwrap_or(&Value::Nil);
                let main = args_iter.next().unwrap_or(&Value::Nil);
                Program {
                    prog_defs: defs.list_iter().unwrap().map(|s| (0, GlobDef::GFun(Function::from(s.to_string())))).collect(),
                    prog_public: public.list_iter().unwrap().map(|s| s.as_u64().unwrap() as usize).collect(),
                    prog_main: main.to_string().parse::<u64>().unwrap() as usize,
                }
            }
            _ => panic!("Failed to parse Program {}", s),
        }
    }
}

#[test]
fn test_load_program(){
    let data = read_file("sample.mach");
    let p = Program::from(data); 
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallingConvention {
    pub cc_vararg: Option<Z>,
    pub cc_unproto: bool,
    pub cc_structret: bool,
}

pub fn mkcallconv(cc_vararg: Option<Z>, cc_unproto: bool, cc_structret: bool) -> CallingConvention {
    CallingConvention {
        cc_vararg,
        cc_unproto,
        cc_structret,
    }
}
