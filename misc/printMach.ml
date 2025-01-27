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
(* Modified to output states *)

open Printf
open Camlcoq
open Datatypes
open AST
open Mach
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

let string_of_ptrofs ofs = string_of_int (Z.to_int ofs)

let string_of_operation op =
  match op with
  | Omove -> "Omove"
  | Ointconst n -> Printf.sprintf "Ointconst(%d)" (Z.to_int n)
  | Olongconst n -> Printf.sprintf "Olongconst(%Ld)" (Int64.of_int (Z.to_int n))
  (* | Ofloatconst n -> Printf.sprintf "Ofloatconst(%f)" n
  | Osingleconst n -> Printf.sprintf "Osingleconst(%f)" n
  | Oindirectsymbol id -> Printf.sprintf "Oindirectsymbol(%d)" (Obj.magic id) *)
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
  | _ -> "Placeholder"

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

let string_of_addressing = function
  | Op.Aindexed ofs -> Printf.sprintf "Aindexed(%s)" (string_of_ptrofs ofs)
  | Op.Aindexed2 ofs -> Printf.sprintf "Aindexed2(%s)" (string_of_ptrofs ofs)
  | Op.Ascaled (scale, ofs) -> Printf.sprintf "Ascaled(%d, %s)" (Z.to_int scale) (string_of_ptrofs ofs)
  | Op.Aindexed2scaled (scale, ofs) -> Printf.sprintf "Aindexed2scaled(%d, %s)" (Z.to_int scale) (string_of_ptrofs ofs)
  (* | Op.Aglobal (id, ofs) -> Printf.sprintf "Aglobal(%s, %s)" (Obj.magic id) (string_of_ptrofs ofs)
  | Op.Abased (id, ofs) -> Printf.sprintf "Abased(%s, %s)" (Obj.magic id) (string_of_ptrofs ofs) *)
  | Op.Abasedscaled (scale, id, ofs) -> Printf.sprintf "Abasedscaled(%d, %s, %s)" (Z.to_int scale) (Obj.magic id) (string_of_ptrofs ofs)
  | Op.Ainstack ofs -> Printf.sprintf "Ainstack(%s)" (string_of_ptrofs ofs)
  | _ -> "Placeholder"

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let string_of_calling_convention cc =
  Printf.sprintf "{vararg: %s, unproto: %s, structret: %s}"
    (match cc.cc_vararg with Some n -> string_of_int (Z.to_int n) | None -> "None")
    (string_of_bool cc.cc_unproto)
    (string_of_bool cc.cc_structret)

let string_of_xtype = function
| Xbool -> "Xbool"
| Xint8signed -> "Xint8signed"
| Xint8unsigned -> "Xint8unsigned"
| Xint16signed -> "Xint16signed"
| Xint16unsigned -> "Xint16unsigned"
| Xint -> "Xint"
| Xfloat -> "Xfloat"
| Xlong -> "Xlong"
| Xsingle -> "Xsingle"
| Xptr -> "Xptr"
| Xany32 -> "Xany32"
| Xany64 -> "Xany64"
| Xvoid -> "Xvoid"


let string_of_signature sig_ =
  let args = List.map (fun x -> string_of_xtype x) sig_.sig_args in
  let res = string_of_xtype sig_.sig_res in
  let cc = sig_.sig_cc in
  Printf.sprintf "{args: [%s], res: %s, sig_cc: %s}" (String.concat ", " args) res (string_of_calling_convention cc)

let string_of_positive p = string_of_int (P.to_int p)

let string_of_comparison = function
| _ -> "string_of_comparison"

let string_of_condition = function
| Ccomp c -> Printf.sprintf "Ccomp(%s)" (string_of_comparison c)
| Ccompu c -> Printf.sprintf "Ccompu(%s)" (string_of_comparison c)
| Ccompimm (c, n) -> Printf.sprintf "Ccompimm(%s, %s)" (string_of_comparison c) (string_of_ptrofs n)
| Ccompuimm (c, n) -> Printf.sprintf "Ccompuimm(%s, %s)" (string_of_comparison c) (string_of_ptrofs n)
| Ccompl c -> Printf.sprintf "Ccompl(%s)" (string_of_comparison c)
| Ccomplu c -> Printf.sprintf "Ccomplu(%s)" (string_of_comparison c)
| Ccomplimm (c, n) -> Printf.sprintf "Ccomplimm(%s, %s)" (string_of_comparison c) (string_of_ptrofs n)
| Ccompluimm (c, n) -> Printf.sprintf "Ccompluimm(%s, %s)" (string_of_comparison c) (string_of_ptrofs n)
| Ccompf c -> Printf.sprintf "Ccompf(%s)" (string_of_comparison c)
| Cnotcompf c -> Printf.sprintf "Cnotcompf(%s)" (string_of_comparison c)
| Ccompfs c -> Printf.sprintf "Ccompfs(%s)" (string_of_comparison c)
| Cnotcompfs c -> Printf.sprintf "Cnotcompfs(%s)" (string_of_comparison c)
| Cmaskzero n -> Printf.sprintf "Cmaskzero(%s)" (string_of_ptrofs n)
| Cmasknotzero n -> Printf.sprintf "Cmasknotzero(%s)" (string_of_ptrofs n)


let rec string_of_builtin_res = function
| BR x -> Printf.sprintf "BR(%s)" (Obj.magic x)
| BR_none -> "BR_none"
| BR_splitlong (hi, lo) -> 
    Printf.sprintf "BR_splitlong(%s, %s)" (string_of_builtin_res hi) (string_of_builtin_res lo)


let string_of_external_function = function
| EF_external (name, sg) -> Printf.sprintf "EF_external(%s, %s)" (string_of_char_list name) (string_of_signature sg)
| EF_builtin (name, sg) -> Printf.sprintf "EF_builtin(%s, %s)" (string_of_char_list name) (string_of_signature sg)
| EF_runtime (name, sg) -> Printf.sprintf "EF_runtime(%s, %s)" (string_of_char_list name) (string_of_signature sg)
| EF_vload chunk -> Printf.sprintf "EF_vload(%s)" (string_of_chunk chunk)
| EF_vstore chunk -> Printf.sprintf "EF_vstore(%s)" (string_of_chunk chunk)
| EF_malloc -> "EF_malloc"
| EF_free -> "EF_free"
| EF_memcpy (sz, al) -> Printf.sprintf "EF_memcpy(%d, %d)" (Z.to_int sz) (Z.to_int al)
| EF_annot (kind, text, targs) -> Printf.sprintf "EF_annot(%s, %s, [%s])" (string_of_positive kind) (string_of_char_list text) (String.concat ", " (List.map (fun x -> Obj.magic x) targs))
| EF_annot_val (kind, text, targ) -> Printf.sprintf "EF_annot_val(%s, %s, %s)" (string_of_positive kind) (string_of_char_list text) (Obj.magic targ)
| EF_inline_asm (text, sg, clobbers) -> Printf.sprintf "EF_inline_asm(%s, %s, [%s])" (string_of_char_list text) (string_of_signature sg) (String.concat ", " (List.map (fun x -> Obj.magic x) clobbers))
| EF_debug (kind, text, targs) -> Printf.sprintf "EF_debug(%s, %s, [%s])" (string_of_positive kind) (Obj.magic text) (String.concat ", " (List.map (fun x -> Obj.magic x) targs))

(* 
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
  | BA_addptr (a1 a2: builtin_arg A). *)

let rec string_of_builtin_arg = function
  | BA x -> Printf.sprintf "BA(%s)" x
  | BA_int n -> Printf.sprintf "BA_int(%s)" (string_of_ptrofs n)
  | BA_long n -> Printf.sprintf "BA_long(%s)" (string_of_ptrofs n)
  | BA_float f -> Printf.sprintf "BA_float(%s)" (Obj.magic f)
  | BA_single f -> Printf.sprintf "BA_single(%f)" (Obj.magic f)
  | BA_loadstack (chunk, ofs) -> Printf.sprintf "BA_loadstack(%s, %s)" (string_of_chunk chunk) (string_of_ptrofs ofs)
  | BA_addrstack ofs -> Printf.sprintf "BA_addrstack(%s)" (string_of_ptrofs ofs)
  | BA_loadglobal (chunk, id, ofs) -> Printf.sprintf "BA_loadglobal(%s, %s, %s)" (string_of_chunk chunk) (Obj.magic id) (string_of_ptrofs ofs)
  | BA_addrglobal (id, ofs) -> Printf.sprintf "BA_addrglobal(%s, %s)" (Obj.magic id) (string_of_ptrofs ofs)
  | BA_splitlong (hi, lo) -> Printf.sprintf "BA_splitlong(%s, %s)" (string_of_builtin_arg hi) (string_of_builtin_arg lo)
  | BA_addptr (a1, a2) -> Printf.sprintf "BA_addptr(%s, %s)" (string_of_builtin_arg a1) (string_of_builtin_arg a2)


let string_of_typ = function
| Tint -> "Tint"
| Tfloat -> "Tfloat"
| Tlong -> "Tlong"
| Tsingle -> "Tsingle"
| Tany32 -> "Tany32"
| Tany64 -> "Tany64"

let string_fn_mcall = function
  | Coq_inl r -> (string_of_mreg r)
  | Coq_inr s -> (extern_atom s)

let print_instruction oc = function
  | Mgetstack (ofs, typ, reg) -> fprintf oc "Mgetstack(%s, %s, %s)\n" (string_of_ptrofs ofs) (string_of_typ typ) (string_of_mreg reg)
  | Msetstack (arg, ofs, typ) -> fprintf oc "Msetstack(%s, %s, %s)\n"  (string_of_mreg arg) (string_of_ptrofs ofs) (string_of_typ typ)
  | Mgetparam (ofs, typ, res) -> 
    fprintf oc "Mgetparam(%s, %s, %s)\n" 
        (string_of_ptrofs ofs) 
        (string_of_typ typ) 
        (string_of_mreg res)
  | Mop (op, args, res) ->
    fprintf oc "Mop(%s, [%s], %s)\n"
      (string_of_operation op)
      (String.concat ", " (List.map string_of_mreg args))
      (string_of_mreg res)
  | Mload (chunk, addr, args, res) -> fprintf oc "Mload(%s, %s, [%s], %s)\n" (string_of_chunk chunk) (string_of_addressing addr) (String.concat ", " (List.map string_of_mreg args)) (string_of_mreg res)
  | Mstore (chunk, addr, args, src) -> fprintf oc "Mstore(%s, %s, [%s], %s)\n" (string_of_chunk chunk) (string_of_addressing addr) (String.concat ", " (List.map string_of_mreg args)) (string_of_mreg src)
  | Mcall (sig_, tgt) -> fprintf oc "Mcall(%s, %s)\n" (string_of_signature sig_) (string_fn_mcall tgt)
  | Mtailcall (sig_, tgt) -> fprintf oc "Mtailcall(%s, %s)\n" (string_of_signature sig_) (string_fn_mcall tgt)
  | Mbuiltin (ef, args, res) -> fprintf oc "Mbuiltin(%s, [%s], %s)\n" (string_of_external_function ef) args (string_of_builtin_res res)
  | Mlabel lbl -> fprintf oc "Mlabel(%s)\n" (string_of_positive lbl)
  | Mgoto lbl -> fprintf oc "Mgoto(%s)\n" (string_of_positive lbl)
  | Mcond (cond, args, lbl) -> fprintf oc "Mcond(%s, [%s], %s)\n" (string_of_condition cond) (String.concat ", " (List.map string_of_mreg args)) (string_of_positive lbl)
  | Mjumptable (reg, lbls) -> fprintf oc "Mjumptable(%s, [%s])\n" (string_of_mreg reg) (String.concat ", " (List.map string_of_positive lbls))
  | Mreturn -> fprintf oc "Mreturn\n"
  | _ -> fprintf oc "Mnotimplemented\n"



(* Function to print a function_ *)
let print_function oc id f =
  fprintf oc "{Function: %s\n"  (extern_atom id);
  fprintf oc "Function Signature: %s\n" (string_of_signature f.fn_sig);
  fprintf oc "Stack Size: %s\n" (string_of_ptrofs f.fn_stacksize);
  fprintf oc "Link Offset: %s\n" (string_of_ptrofs f.fn_link_ofs);
  fprintf oc "Return Address Offset: %s\n" (string_of_ptrofs f.fn_retaddr_ofs);
  fprintf oc "Code:\n";
  List.iter (print_instruction oc) f.fn_code;
  fprintf oc "End Function: %s\n\n"  (extern_atom id)

(* Function to print a global definition *)
let print_globdef pp (id, gd) =
  match gd with
  | Gfun(Internal f) -> print_function pp id f
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

