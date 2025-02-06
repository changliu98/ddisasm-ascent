Record function: Type := mkfunction
  { fn_sig: signature;
    fn_code: code;
    fn_stacksize: Z;
    fn_link_ofs: ptrofs;
    fn_retaddr_ofs: ptrofs }.

Definition code := list instruction.
Inductive instruction: Type :=
  | Mgetstack: ptrofs -> typ -> mreg -> instruction
  | Msetstack: mreg -> ptrofs -> typ -> instruction
  | Mgetparam: ptrofs -> typ -> mreg -> instruction
  | Mop: operation -> list mreg -> mreg -> instruction
  | Mload: memory_chunk -> addressing -> list mreg -> mreg -> instruction
  | Mstore: memory_chunk -> addressing -> list mreg -> mreg -> instruction
  | Mcall: signature -> mreg + ident -> instruction
  | Mtailcall: signature -> mreg + ident -> instruction
  | Mbuiltin: external_function -> list (builtin_arg mreg) -> builtin_res mreg -> instruction
  | Mlabel: label -> instruction
  | Mgoto: label -> instruction
  | Mcond: condition -> list mreg -> label -> instruction
  | Mjumptable: mreg -> list label -> instruction
  | Mreturn: instruction.

Definition label := positive.
Definition ptrofs := Z.
Inductive mreg: Type :=
  (** Allocatable integer regs *)
  | AX | BX | CX | DX | SI | DI | BP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15  (**r only in 64-bit mode *)
  (** Allocatable float regs *)
  | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15  (**r only in 64-bit mode *)
  (** Special float reg *)
  | FP0.

Inductive builtin_arg (A: Type) : Type :=
  | BA (x: A)
  | BA_int (n: int)
  | BA_long (n: int64)
  | BA_float (f: float)
  | BA_single (f: float32)
  | BA_loadstack (chunk: memory_chunk) (ofs: ptrofs)
  | BA_addrstack (ofs: ptrofs)
  | BA_loadglobal (chunk: memory_chunk) (id: ident) (ofs: ptrofs)
  | BA_addrglobal (id: ident) (ofs: ptrofs)
  | BA_splitlong (hi lo: builtin_arg A)
  | BA_addptr (a1 a2: builtin_arg A).

Definition ident := positive.
Inductive typ : Type :=
  | Tint                (**r 32-bit integers or pointers *)
  | Tfloat              (**r 64-bit double-precision floats *)
  | Tlong               (**r 64-bit integers *)
  | Tsingle             (**r 32-bit single-precision floats *)
  | Tany32              (**r any 32-bit value *)
  | Tany64.             (**r any 64-bit value, i.e. any value *)




Inductive operation : Type :=
  | Omove                    (**r [rd = r1] *)
  | Ointconst (n: int)       (**r [rd] is set to the given integer constant *)
  | Olongconst (n: int64)    (**r [rd] is set to the given integer constant *)
  | Ofloatconst (n: float)   (**r [rd] is set to the given float constant *)
  | Osingleconst (n: float32)(**r [rd] is set to the given float constant *)
  | Oindirectsymbol (id: ident) (**r [rd] is set to the address of the symbol *)
  (*c 32-bit integer arithmetic: *)
  | Ocast8signed             (**r [rd] is 8-bit sign extension of [r1] *)
  | Ocast8unsigned           (**r [rd] is 8-bit zero extension of [r1] *)
  | Ocast16signed            (**r [rd] is 16-bit sign extension of [r1] *)
  | Ocast16unsigned          (**r [rd] is 16-bit zero extension of [r1] *)
  | Oneg                     (**r [rd = - r1] *)
  | Osub                     (**r [rd = r1 - r2] *)
  | Omul                     (**r [rd = r1 * r2] *)
  | Omulimm (n: int)         (**r [rd = r1 * n] *)
  | Omulhs                   (**r [rd = high part of r1 * r2, signed] *)
  | Omulhu                   (**r [rd = high part of r1 * r2, unsigned] *)
  | Odiv                     (**r [rd = r1 / r2] (signed) *)
  | Odivu                    (**r [rd = r1 / r2] (unsigned) *)
  | Omod                     (**r [rd = r1 % r2] (signed) *)
  | Omodu                    (**r [rd = r1 % r2] (unsigned) *)
  | Oand                     (**r [rd = r1 & r2] *)
  | Oandimm (n: int)         (**r [rd = r1 & n] *)
  | Oor                      (**r [rd = r1 | r2] *)
  | Oorimm (n: int)          (**r [rd = r1 | n] *)
  | Oxor                     (**r [rd = r1 ^ r2] *)
  | Oxorimm (n: int)         (**r [rd = r1 ^ n] *)
  | Onot                     (**r [rd = ~r1] *)
  | Oshl                     (**r [rd = r1 << r2] *)
  | Oshlimm (n: int)         (**r [rd = r1 << n] *)
  | Oshr                     (**r [rd = r1 >> r2] (signed) *)
  | Oshrimm (n: int)         (**r [rd = r1 >> n] (signed) *)
  | Oshrximm (n: int)        (**r [rd = r1 / 2^n] (signed) *)
  | Oshru                    (**r [rd = r1 >> r2] (unsigned) *)
  | Oshruimm (n: int)        (**r [rd = r1 >> n] (unsigned) *)
  | Ororimm (n: int)         (**r rotate right immediate *)
  | Oshldimm (n: int)        (**r [rd = r1 << n | r2 >> (32-n)] *)
  | Olea (a: addressing)     (**r effective address *)
  (*c 64-bit integer arithmetic: *)
  | Omakelong                (**r [rd = r1 << 32 | r2] *)
  | Olowlong                 (**r [rd = low-word(r1)] *)
  | Ohighlong                (**r [rd = high-word(r1)] *)
  | Ocast32signed            (**r [rd] is 32-bit sign extension of [r1] *)
  | Ocast32unsigned          (**r [rd] is 32-bit zero extension of [r1] *)
  | Onegl                    (**r [rd = - r1] *)
  | Oaddlimm (n: int64)      (**r [rd = r1 + n] *)
  | Osubl                    (**r [rd = r1 - r2] *)
  | Omull                    (**r [rd = r1 * r2] *)
  | Omullimm (n: int64)      (**r [rd = r1 * n] *)
  | Omullhs                  (**r [rd = high part of r1 * r2, signed] *)
  | Omullhu                  (**r [rd = high part of r1 * r2, unsigned] *)
  | Odivl                    (**r [rd = r1 / r2] (signed) *)
  | Odivlu                   (**r [rd = r1 / r2] (unsigned) *)
  | Omodl                    (**r [rd = r1 % r2] (signed) *)
  | Omodlu                   (**r [rd = r1 % r2] (unsigned) *)
  | Oandl                    (**r [rd = r1 & r2] *)
  | Oandlimm (n: int64)      (**r [rd = r1 & n] *)
  | Oorl                     (**r [rd = r1 | r2] *)
  | Oorlimm (n: int64)       (**r [rd = r1 | n] *)
  | Oxorl                    (**r [rd = r1 ^ r2] *)
  | Oxorlimm (n: int64)      (**r [rd = r1 ^ n] *)
  | Onotl                    (**r [rd = ~r1] *)
  | Oshll                    (**r [rd = r1 << r2] *)
  | Oshllimm (n: int)        (**r [rd = r1 << n] *)
  | Oshrl                    (**r [rd = r1 >> r2] (signed) *)
  | Oshrlimm (n: int)        (**r [rd = r1 >> n] (signed) *)
  | Oshrxlimm (n: int)       (**r [rd = r1 / 2^n] (signed) *)
  | Oshrlu                   (**r [rd = r1 >> r2] (unsigned) *)
  | Oshrluimm (n: int)       (**r [rd = r1 >> n] (unsigned) *)
  | Ororlimm (n: int)        (**r rotate right immediate *)
  | Oleal (a: addressing)    (**r effective address *)
  (*c Floating-point arithmetic: *)
  | Onegf                    (**r [rd = - r1] *)
  | Oabsf                    (**r [rd = abs(r1)] *)
  | Oaddf                    (**r [rd = r1 + r2] *)
  | Osubf                    (**r [rd = r1 - r2] *)
  | Omulf                    (**r [rd = r1 * r2] *)
  | Odivf                    (**r [rd = r1 / r2] *)
  | Omaxf                    (**r [rd = max(r1,r2)] *)
  | Ominf                    (**r [rd = min(r1,r2)] *)
  | Onegfs                   (**r [rd = - r1] *)
  | Oabsfs                   (**r [rd = abs(r1)] *)
  | Oaddfs                   (**r [rd = r1 + r2] *)
  | Osubfs                   (**r [rd = r1 - r2] *)
  | Omulfs                   (**r [rd = r1 * r2] *)
  | Odivfs                   (**r [rd = r1 / r2] *)
  | Osingleoffloat           (**r [rd] is [r1] truncated to single-precision float *)
  | Ofloatofsingle           (**r [rd] is [r1] extended to double-precision float *)
  (*c Conversions between int and float: *)
  | Ointoffloat              (**r [rd = signed_int_of_float64(r1)] *)
  | Ofloatofint              (**r [rd = float64_of_signed_int(r1)] *)
  | Ointofsingle             (**r [rd = signed_int_of_float32(r1)] *)
  | Osingleofint             (**r [rd = float32_of_signed_int(r1)] *)
  | Olongoffloat             (**r [rd = signed_long_of_float64(r1)] *)
  | Ofloatoflong             (**r [rd = float64_of_signed_long(r1)] *)
  | Olongofsingle            (**r [rd = signed_long_of_float32(r1)] *)
  | Osingleoflong            (**r [rd = float32_of_signed_long(r1)] *)
  (*c Boolean tests: *)
  | Ocmp (cond: condition)  (**r [rd = 1] if condition holds, [rd = 0] otherwise. *)
  | Osel: condition -> typ -> operation.
                              (**r [rd = rs1] if condition holds, [rd = rs2] otherwise. *)



Record signature : Type := mksignature {
    sig_args: list xtype;
    sig_res: xtype;
    sig_cc: calling_convention
  }.

Inductive xtype : Type :=
| Xbool               (**r Boolean value (0 or 1) *)
| Xint8signed         (**r 8-bit signed integer *)
| Xint8unsigned       (**r 8-bit unsigned integer *)
| Xint16signed        (**r 16-bit signed integer *)
| Xint16unsigned      (**r 16-bit unsigned integer *)
| Xint                (**r 32-bit integers or pointers *)
| Xfloat              (**r 64-bit double-precision floats *)
| Xlong               (**r 64-bit integers *)
| Xsingle             (**r 32-bit single-precision floats *)
| Xptr                (**r pointers and pointer-sized integers *)
| Xany32              (**r any 32-bit value *)
| Xany64              (**r any 64-bit value, i.e. any value *)
| Xvoid.              (**r no meaningful value *)

Record calling_convention : Type := mkcallconv {
  cc_vararg: option Z;  (**r variable-arity function (+ number of fixed args) *)
  cc_unproto: bool;     (**r old-style unprototyped function *)
  cc_structret: bool    (**r function returning a struct  *)
}.
