use crate::x86::op::Ptrofs;

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

pub type Ident = usize;

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
    EFMemcpy(i64, i64),
    EFAnnot(i64, String, Vec<Typ>),
    EFAnnotVal(i64, String, Typ),
    EFInlineAsm(String, Signature, Vec<String>),
    EFDebug(i64, Ident, Vec<Typ>),
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
