use std::hash::{Hash, Hasher};
use lexpr::{Value, parse::Error, cons};
use crate::x86::op::Ptrofs;


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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XType {
    XBool,
    XInt8Signed,
    XInt8Unsigned,
    XInt16Signed,
    XInt16Unsigned,
    XInt,
    XFloat,
    XLong,
    XSingle,
    XPtr,
    XAny32,
    XAny64,
    XVoid,
}

impl From<String> for XType {
    fn from(s: String) -> Self {
        match s.as_str() {
            "AST.xtype.bool" => XType::XBool, "AST.xtype.int8" => XType::XInt8Signed, "AST.xtype.uint8" => XType::XInt8Unsigned,
            "AST.xtype.int16" => XType::XInt16Signed,"AST.xtype.uint16" => XType::XInt16Unsigned, "AST.xtype.int" => XType::XInt,
            "AST.xtype.float" => XType::XFloat, "AST.xtype.long" => XType::XLong, "AST.xtype.single" => XType::XSingle,
            "AST.xtype.ptr" => XType::XPtr, "AST.xtype.any32" => XType::XAny32, "AST.xtype.any64" => XType::XAny64,
            "AST.xtype.void" => XType::XVoid,
            _ => panic!("Invalid type"),
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
    pub args: Vec<XType>,
    pub res: XType,
    pub cc: CallConv,
}
impl Default for Signature {
    fn default() -> Self {
        Signature {
            args: vec![],
            res: XType::XVoid,
            cc: CallConv::default(),
        }
    }
}
impl From<String> for Signature {
    fn from(s: String) -> Self {
        let parsed = lexpr::from_str(&s).expect("Failed to parse");
        match parsed {
            Value::Cons(cons) => {
                let head = cons.car();
                if *head != Value::Symbol("mksignature".into()) {
                    panic!("Failed to parse: {}, expected mksignature", head)
                }
                let args = cons.cdr();
                let mut args = args.list_iter().unwrap();
                let res = args.next().unwrap().to_string();
                let cc = args.next().unwrap().to_string();
                Signature {
                    args: args.into_iter().map(|s| XType::from(s)).collect(),
                    res: XType::from(res),
                    cc: CallConv::default(),
                }
            }
            _ => panic!("Failed to parse {}", s),
        }
    }
}


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


impl<F, V> Program<F, V> {
    pub fn mkprogram(prog_defs: Vec<(Ident, GlobDef<F, V>)>, prog_public: Vec<Ident>, prog_main: Ident) -> Self {
        Program {
            prog_defs,
            prog_public,
            prog_main,
        }
    }
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
