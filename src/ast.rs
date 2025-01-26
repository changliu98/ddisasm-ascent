use std::hash::{Hash, Hasher};

use crate::x86::op::Ptrofs;


pub type Ident = usize;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Typ {
    TInt,
    TFloat,
    TLong,
    TSingle,
    TAny32,
    TAny64,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallConv {
    pub varargs: Option<i64>,
    pub unproto: bool,
    pub structured_ret: bool,
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
