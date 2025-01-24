// AArch64 General Purpose Registers

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IReg {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,
}

// Sum type
// General Purpose Registers + Zero Register
// The XZR (zero register) is a read-only register that always returns 0 when read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IReg0 {
    RR0(IReg),
    XZR,
}

// coercion from IReg to IReg0
impl From<IReg> for IReg0 {
    fn from(reg: IReg) -> Self {
        IReg0::RR0(reg)
    }
}

// Sum type
// General Purpose Registers + Stack Pointer
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IRegSP {
    RR1(IReg),
    SP,
}

// coercion from IReg to IRegSP
impl From<IReg> for IRegSP {
    fn from(reg: IReg) -> Self {
        IRegSP::RR1(reg)
    }
}


// float pointer registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FReg {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    D9,
    D10,
    D11,
    D12,
    D13,
    D14,
    D15,
    D16,
    D17,
    D18,
    D19,
    D20,
    D21,
    D22,
    D23,
    D24,
    D25,
    D26,
    D27,
    D28,
    D29,
    D30,
    D31,
}

// condition code bits in the Current Program Status Register (CPSR)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CrBit {
    CN, // Negative
    CZ, // Zero
    CV, // Overflow
    CC, // Carry
}

// general purpose registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PReg {
    IR(IReg),  // integer register
    FR(FReg),  // float register
    CR(CrBit), // condition code bit
    SP,        // stack pointer
    PC,        // program counter
}

// coercion from IReg to PReg
impl From<IReg> for PReg {
    fn from(reg: IReg) -> Self {
        PReg::IR(reg)
    }
}

// coercion from FReg to PReg
impl From<FReg> for PReg {
    fn from(reg: FReg) -> Self {
        PReg::FR(reg)
    }
}

// coercion from CrBit to PReg
impl From<CrBit> for PReg {
    fn from(reg: CrBit) -> Self {
        PReg::CR(reg)
    }
}

pub fn preg_of_iregsp(iregsp: IRegSP) -> PReg {
    match iregsp {
        IRegSP::RR1(ireg) => PReg::IR(ireg),
        IRegSP::SP => PReg::SP,
    }
}

// coercion from IRegSP to PReg
impl From<IRegSP> for PReg {
    fn from(iregsp: IRegSP) -> Self {
        preg_of_iregsp(iregsp)
    }
}

// ISA

pub type Label = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum isize {
    W, X
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum fsize {
    S, D
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TestCond {
    TCeq, // Equal
    TCne, // Not equal
    TClt, // Less than
    TCle, // Less than or equal
    TCgt, // Greater than
    TCge, // Greater than or equal
    TCmi, // Minus (negative)
    TCpl, // Plus (zero or positive)
    TCvs, // Overflow
    TCvc, // No overflow
    TCcs, // Carry set
    TCcc, // Carry clear
    TChi, // Unsigned higher
    TCls, // Unsigned lower or same
}

type ident = usize;
type ptofs = i64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Addressing {
    ADimm(IRegSP, i64),         // base plus immediate offset
    ADreg(IRegSP, IReg),        // base plus registe
    ADlsl(IRegSP, IReg, i32),    // base plus register with shift
    ADsxt(IRegSP, IReg, i32),  // base plus register with sign extension
    ADuxt(IRegSP, IReg, i32), // base plus register with zero extension
    ADadr(IRegSP, ident, ptofs), // base plus label offset
    ADpostincr(IRegSP, i64),   // post-increment
}