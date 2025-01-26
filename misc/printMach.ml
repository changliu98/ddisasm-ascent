(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(** Pretty-printer for Mach code *)

open Printf
open Camlcoq
open Datatypes
open AST
open Mach
open PrintAST
open Op

let reg pp r =
  match Machregsnames.name_of_register r with
  | Some s -> fprintf pp "%s" s
  | None -> fprintf pp "<unknown reg>"

let rec regs pp = function
  | [] -> ()
  | [r] -> reg pp r
  | r1::rl -> fprintf pp "%a, %a" reg r1 regs rl

let ros pp = function
  | Coq_inl r -> reg pp r
  | Coq_inr s -> fprintf pp "\"%s\"" (extern_atom s)

let print_instruction pp i =
  match i with
  | Mgetstack(ofs, ty, res) ->
      fprintf pp "\t%a = stack(%ld, %s)\n"
              reg res (camlint_of_coqint ofs) (name_of_type ty)
  | Msetstack(arg, ofs, ty) ->
      fprintf pp "\tstack(%ld, %s) = %a\n"
              (camlint_of_coqint ofs) (name_of_type ty) reg arg
  | Mgetparam(ofs, ty, res) ->
      fprintf pp "\t%a = param(%ld, %s)\n"
              reg res (camlint_of_coqint ofs) (name_of_type ty)
  | Mop(op, args, res) ->
      fprintf pp "\t%a = %a\n"
         reg res (PrintOp.print_operation reg) (op, args)
  | Mload(chunk, addr, args, dst) ->
      fprintf pp "\t%a = %s[%a]\n"
         reg dst (name_of_chunk chunk)
         (PrintOp.print_addressing reg) (addr, args)
  | Mstore(chunk, addr, args, src) ->
      fprintf pp "\t%s[%a] = %a\n"
         (name_of_chunk chunk)
         (PrintOp.print_addressing reg) (addr, args)
         reg src
  | Mcall(sg, fn) ->
      fprintf pp "\tcall %a\n" ros fn
  | Mtailcall(sg, fn) ->
      fprintf pp "\ttailcall %a\n" ros fn
  | Mbuiltin(ef, args, res) ->
      fprintf pp "\t%a = %s(%a)\n"
        (print_builtin_res reg) res
        (name_of_external ef)
        (print_builtin_args reg) args
  | Mlabel lbl ->
      fprintf pp "%5d:" (P.to_int lbl)
  | Mgoto lbl ->
      fprintf pp "\tgoto %d\n" (P.to_int lbl)
  | Mcond(cond, args, lbl) ->
      fprintf pp "\tif (%a) goto %d\n"
        (PrintOp.print_condition reg) (cond, args)
        (P.to_int lbl)
  | Mjumptable(arg, tbl) ->
      let tbl = Array.of_list tbl in
      fprintf pp "\tjumptable (%a)\n" reg arg;
      for i = 0 to Array.length tbl - 1 do
        fprintf pp "\t\tcase %d: goto %d\n" i (P.to_int tbl.(i))
      done
  | Mreturn ->
      fprintf pp "\treturn\n"


let print_function pp id f =
  fprintf pp "%s() {\n" (extern_atom id);
  List.iter (print_instruction pp) f.fn_code;
  fprintf pp "}\n\n"

let destination : string option ref = ref None


type typ =
  | Tint
  | Tfloat
  | Tlong
  | Tsingle
  | Tany32
  | Tany64

type ptrofs = int  (* Pointer offsets *)
type label = int   (* Labels *)

type mreg =
  | AX | BX | CX | DX | SI | DI | BP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
  | FP0

type builtin_arg =
  | BA of string
  | BA_int of int
  | BA_long of int64
  | BA_float of float
  | BA_single of float
  | BA_loadstack of string * ptrofs
  | BA_addrstack of ptrofs
  | BA_loadglobal of string * string * ptrofs
  | BA_addrglobal of string * ptrofs
  | BA_splitlong of builtin_arg * builtin_arg
  | BA_addptr of builtin_arg * builtin_arg

type instruction =
  | Mgetstack of ptrofs * typ * mreg
  | Msetstack of mreg * ptrofs * typ
  | Mgetparam of ptrofs * typ * mreg
  | Mop of string * mreg list * mreg
  | Mload of string * string * mreg list * mreg
  | Mstore of string * string * mreg list * mreg
  | Mcall of string * (mreg option)
  | Mtailcall of string * (mreg option)
  | Mbuiltin of string * builtin_arg list * string
  | Mlabel of label
  | Mgoto of label
  | Mcond of string * mreg list * label
  | Mjumptable of mreg * label list
  | Mreturn

type code = instruction list

type function_ = {
  fn_sig: string;
  fn_code: code;
  fn_stacksize: int;
  fn_link_ofs: ptrofs;
  fn_retaddr_ofs: ptrofs;
}

type globdef =
  | Gfun of function_

type program = {
  prog_defs: (string * globdef) list;
}

let string_of_ptrofs ofs = string_of_int (Z.to_int ofs)

let string_of_operation op =
  match op with
  | Omove -> "Omove"
  | Ointconst n -> Printf.sprintf "Ointconst(%d)" (Z.to_int n)
  | Olongconst n -> Printf.sprintf "Olongconst(%Ld)" (Int64.of_int (Z.to_int n))
  | Ofloatconst n -> Printf.sprintf "Ofloatconst(%f)" (Obj.magic n)
  | Osingleconst n -> Printf.sprintf "Osingleconst(%f)" (Obj.magic n)
  | Oindirectsymbol id -> Printf.sprintf "Oindirectsymbol(%d)" (Obj.magic id)
  | Ocast8signed -> "Ocast8signed"
  | Ocast8unsigned -> "Ocast8unsigned"
  | Ocast16signed -> "Ocast16signed"
  | Ocast16unsigned -> "Ocast16unsigned"
  | Oneg -> "Oneg"
  | Osub -> "Osub"
  | Omul -> "Omul"
  | Omulimm n -> Printf.sprintf "Omulimm(%d)" (Z.to_int n)
  | Omulhs -> "Omulhs"
  | Omulhu -> "Omulhu"
  | Odiv -> "Odiv"
  | Odivu -> "Odivu"
  | Omod -> "Omod"
  | Omodu -> "Omodu"
  | Oand -> "Oand"
  | Oandimm n -> Printf.sprintf "Oandimm(%d)" (Z.to_int n)
  | Oor -> "Oor"
  | Oorimm n -> Printf.sprintf "Oorimm(%d)" (Z.to_int n)
  | Oxor -> "Oxor"
  | Oxorimm n -> Printf.sprintf "Oxorimm(%d)" (Z.to_int n)
  | Onot -> "Onot"
  | Oshl -> "Oshl"
  | Oshlimm n -> Printf.sprintf "Oshlimm(%d)" (Z.to_int n)
  | Oshr -> "Oshr"
  | Oshrimm n -> Printf.sprintf "Oshrimm(%d)" (Z.to_int n)
  | Oshrximm n -> Printf.sprintf "Oshrximm(%d)" (Z.to_int n)
  | Oshru -> "Oshru"
  | Oshruimm n -> Printf.sprintf "Oshruimm(%d)" (Z.to_int n)
  | Ororimm n -> Printf.sprintf "Ororimm(%d)" (Z.to_int n)
  | Oshldimm n -> Printf.sprintf "Oshldimm(%d)" (Z.to_int n)
  | Olea _ -> "Olea(effective address)" (* Replace `_` with specific printing for addressing if needed *)
  | Omakelong -> "Omakelong"
  | Olowlong -> "Olowlong"
  | Ohighlong -> "Ohighlong"
  | Ocast32signed -> "Ocast32signed"
  | Ocast32unsigned -> "Ocast32unsigned"
  | Onegl -> "Onegl"
  | Oaddlimm n -> Printf.sprintf "Oaddlimm(%Ld)" (Int64.of_int (Z.to_int n))
  | Osubl -> "Osubl"
  | Omull -> "Omull"
  | Omullimm n -> Printf.sprintf "Omullimm(%Ld)" (Int64.of_int (Z.to_int n))
  | Omullhs -> "Omullhs"
  | Omullhu -> "Omullhu"
  | Odivl -> "Odivl"
  | Odivlu -> "Odivlu"
  | Omodl -> "Omodl"
  | Omodlu -> "Omodlu"
  | Oandl -> "Oandl"
  | Oandlimm n -> Printf.sprintf "Oandlimm(%Ld)" (Int64.of_int (Z.to_int n))
  | Oorl -> "Oorl"
  | Oorlimm n -> Printf.sprintf "Oorlimm(%Ld)" (Int64.of_int (Z.to_int n))
  | Oxorl -> "Oxorl"
  | Oxorlimm n -> Printf.sprintf "Oxorlimm(%Ld)" (Int64.of_int (Z.to_int n))
  | Onotl -> "Onotl"
  | Oshll -> "Oshll"
  | Oshllimm n -> Printf.sprintf "Oshllimm(%d)" (Z.to_int n)
  | Oshrl -> "Oshrl"
  | Oshrlimm n -> Printf.sprintf "Oshrlimm(%d)" (Z.to_int n)
  | Oshrxlimm n -> Printf.sprintf "Oshrxlimm(%d)" (Z.to_int n)
  | Oshrlu -> "Oshrlu"
  | Oshrluimm n -> Printf.sprintf "Oshrluimm(%d)" (Z.to_int n)
  | Ororlimm n -> Printf.sprintf "Ororlimm(%Ld)" (Int64.of_int (Z.to_int n))
  | Oleal _ -> "Oleal(effective address)" (* Replace `_` with specific printing for addressing if needed *)
  | Onegf -> "Onegf"
  | Oabsf -> "Oabsf"
  | Oaddf -> "Oaddf"
  | Osubf -> "Osubf"
  | Omulf -> "Omulf"
  | Odivf -> "Odivf"
  | Omaxf -> "Omaxf"
  | Ominf -> "Ominf"
  | Onegfs -> "Onegfs"
  | Oabsfs -> "Oabsfs"
  | Oaddfs -> "Oaddfs"
  | Osubfs -> "Osubfs"
  | Omulfs -> "Omulfs"
  | Odivfs -> "Odivfs"
  | Osingleoffloat -> "Osingleoffloat"
  | Ofloatofsingle -> "Ofloatofsingle"
  | Ointoffloat -> "Ointoffloat"
  | Ofloatofint -> "Ofloatofint"
  | Ointofsingle -> "Ointofsingle"
  | Osingleofint -> "Osingleofint"
  | Olongoffloat -> "Olongoffloat"
  | Ofloatoflong -> "Ofloatoflong"
  | Olongofsingle -> "Olongofsingle"
  | Osingleoflong -> "Osingleoflong"
  | Ocmp _ -> "Ocmp(condition)"
  | Osel (_, _) -> "Osel(condition, typ)"


let string_of_mreg = function
  | Machregs.AX -> "AX"
  | Machregs.BX -> "BX"
  | Machregs.CX -> "CX"
  | Machregs.DX -> "DX"
  | Machregs.SI -> "SI"
  | Machregs.DI -> "DI"
  | Machregs.BP -> "BP"
  | Machregs.R8 -> "R8"
  | Machregs.R9 -> "R9"
  | Machregs.R10 -> "R10"
  | Machregs.R11 -> "R11"
  | Machregs.R12 -> "R12"
  | Machregs.R13 -> "R13"
  | Machregs.R14 -> "R14"
  | Machregs.R15 -> "R15"
  | Machregs.X0 -> "X0"
  | Machregs.X1 -> "X1"
  | Machregs.X2 -> "X2"
  | Machregs.X3 -> "X3"
  | Machregs.X4 -> "X4"
  | Machregs.X5 -> "X5"
  | Machregs.X6 -> "X6"
  | Machregs.X7 -> "X7"
  | Machregs.X8 -> "X8"
  | Machregs.X9 -> "X9"
  | Machregs.X10 -> "X10"
  | Machregs.X11 -> "X11"
  | Machregs.X12 -> "X12"
  | Machregs.X13 -> "X13"
  | Machregs.X14 -> "X14"
  | Machregs.X15 -> "X15"
  | Machregs.FP0 -> "FP0"

let string_of_chunk = function
  | Mbool -> "Mbool"
  | Mint8signed -> "Mint8signed"
  | Mint8unsigned -> "Mint8unsigned"
  | Mint16signed -> "Mint16signed"
  | Mint16unsigned -> "Mint16unsigned"
  | Mint32 -> "Mint32"
  | Mint64 -> "Mint64"
  | Mfloat32 -> "Mfloat32"
  | Mfloat64 -> "Mfloat64"
  | Many32 -> "Many32"
  | Many64 -> "Many64"
(* 
  Inductive addressing: Type :=
  | Aindexed: Z -> addressing       (**r Address is [r1 + offset] *)
  | Aindexed2: Z -> addressing      (**r Address is [r1 + r2 + offset] *)
  | Ascaled: Z -> Z -> addressing   (**r Address is [r1 * scale + offset] *)
  | Aindexed2scaled: Z -> Z -> addressing
                                    (**r Address is [r1 + r2 * scale + offset] *)
  | Aglobal: ident -> ptrofs -> addressing (**r Address is [symbol + offset] *)
  | Abased: ident -> ptrofs -> addressing  (**r Address is [symbol + offset + r1] *)
  | Abasedscaled: Z -> ident -> ptrofs -> addressing  (**r Address is [symbol + offset + r1 * scale] *)
  | Ainstack: ptrofs -> addressing. *r Address is [stack_pointer + offset] *)

let string_of_addressing = function
  | Op.Aindexed ofs -> Printf.sprintf "Aindexed(%s)" (string_of_ptrofs ofs)
  | Op.Aindexed2 ofs -> Printf.sprintf "Aindexed2(%s)" (string_of_ptrofs ofs)
  | Op.Ascaled (scale, ofs) -> Printf.sprintf "Ascaled(%d, %s)" (Z.to_int scale) (string_of_ptrofs ofs)
  | Op.Aindexed2scaled (scale, ofs) -> Printf.sprintf "Aindexed2scaled(%d, %s)" (Z.to_int scale) (string_of_ptrofs ofs)
  | Op.Aglobal (id, ofs) -> Printf.sprintf "Aglobal(%s, %s)" (Obj.magic id) (string_of_ptrofs ofs)
  | Op.Abased (id, ofs) -> Printf.sprintf "Abased(%s, %s)" (Obj.magic id) (string_of_ptrofs ofs)
  | Op.Abasedscaled (scale, id, ofs) -> Printf.sprintf "Abasedscaled(%d, %s, %s)" (Z.to_int scale) (Obj.magic id) (string_of_ptrofs ofs)
  | Op.Ainstack ofs -> Printf.sprintf "Ainstack(%s)" (string_of_ptrofs ofs)

(* Record signature : Type := mksignature {
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
}. *)

let string_of_signature sig_ =
  let args = List.map (fun x -> Obj.magic x) sig_.sig_args in
  let res = Obj.magic sig_.sig_res in
  Printf.sprintf "{args: [%s], res: %s}" (String.concat ", " args) res

(* Function to print a single builtin argument *)

(* Function to print a single instruction *)
let print_instruction oc = function
  | Mach.Mgetstack (ofs, typ, reg) -> fprintf oc "Mgetstack(%s, %s, %s)\n" (string_of_ptrofs ofs)  (Obj.magic typ) (Obj.magic reg)
  | Mach.Msetstack (reg, ofs, typ) -> fprintf oc "Msetstack(%s, %s, %s)\n" (Obj.magic reg) (string_of_ptrofs ofs) (Obj.magic typ)
  | Mach.Mgetparam (ofs, typ, reg) -> fprintf oc "Mgetparam(%s, %s, %s)\n" (string_of_ptrofs ofs) (Obj.magic typ) (Obj.magic reg)
  (* | Mach.Mop (op, args, res) ->
    fprintf oc "Mop(%s, [%s], %s)\n"
      (string_of_operation op)
      (String.concat ", " (List.map string_of_mreg args))
      (string_of_mreg res)
  | Mach.Mload (chunk, addr, args, res) -> fprintf oc "Mload(%s, %s, [%s], %s)\n" (string_of_chunk chunk) (string_of_addressing addr) (String.concat ", " (List.map Obj.magic args)) (Obj.magic res)
  | Mach.Mstore (chunk, addr, args, src) -> fprintf oc "Mstore(%s, %s, [%s], %s)\n" (string_of_chunk chunk) (string_of_addressing addr) (String.concat ", " (List.map Obj.magic args)) (Obj.magic src)
  | Mach.Mcall (sig_, tgt) -> fprintf oc "Mcall(%s, %s)\n" (string_of_signature sig_) (match tgt with Some r -> Obj.magic r | None -> "None")
  | Mach.Mtailcall (sig_, tgt) -> fprintf oc "Mtailcall(%s, %s)\n" sig_ (match tgt with Some r -> Obj.magic r | None -> "None")
  | Mach.Mbuiltin (ef, args, res) -> fprintf oc "Mbuiltin(%s, [%s], %s)\n" ef (String.concat ", " (List.map Obj.magic args)) res
  | Mach.Mlabel lbl -> fprintf oc "Mlabel(%d)\n" lbl
  | Mach.Mgoto lbl -> fprintf oc "Mgoto(%d)\n" lbl
  | Mach.Mcond (cond, args, lbl) -> fprintf oc "Mcond(%s, [%s], %d)\n" cond (String.concat ", " (List.map Obj.magic args)) lbl
  | Mach.Mjumptable (reg, lbls) -> fprintf oc "Mjumptable(%s, [%s])\n" (Obj.magic reg) (String.concat ", " (List.map string_of_int lbls))
  | Mach.Mreturn -> fprintf oc "Mreturn\n" *)
  | _ -> fprintf oc "Mreturn\n"

(* Function to print a function_ *)
let print_function oc f =
  fprintf oc "Function Signature: %s\n" f.fn_sig;
  fprintf oc "Stack Size: %d\n" f.fn_stacksize;
  fprintf oc "Link Offset: %d\n" f.fn_link_ofs;
  fprintf oc "Return Address Offset: %d\n" f.fn_retaddr_ofs;
  fprintf oc "Code:\n";
  List.iter (print_instruction oc) f.fn_code;
  fprintf oc "\n"

(* Function to print a global definition *)
let print_globdef oc (id, gd) =
  match gd with
  | Gfun f -> fprintf oc "Global Definition: %s\n" id; print_function oc f
  | _ -> ()

(* Function to print the entire program *)
let print_program oc prog =
  List.iter (print_globdef oc) prog.prog_defs

(* Destination file option *)
let destination : string option ref = ref None

(* Function to print the program if destination is set *)
let print_if prog =
  match !destination with
  | None -> ()
  | Some f ->
      let oc = open_out f in
      print_program oc prog;
      close_out oc


