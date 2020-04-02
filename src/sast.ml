(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStrLit of string
  | SMut of sexpr
  | SNew of sexpr
  | SVar of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  | SPrint of string
  | SCall of string * sexpr list
  | SLet of string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | For of sexpr list * sstmt
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list
