(* Abstract Syntax Tree and functions for printing it *)

type typ = 
    | Int | Char | Float | Bool | String
    | Tvar of string
    | Tarrow of (typ * typ)

type op = 
    | Add | Sub | Mult | Div | Pow | Mod
    | FAdd | FSub | FMult | FDiv | FPow
    | And | Or | Not
    | Eq | FEq | Neq | FNeq | Lt | FLt | Gt | FGt
    | Leq | FLeq | Geq | FGeq 


type expr = 
    | IntLit of int | FloatLit of float | BoolLit of bool
    | CharLit of char | StrLit of string
    | Var of string
    | Mut of expr
    | New of expr
    | Binop of expr * op * expr
    | Print of string
    | Call of string * expr list
    | Let of (assign * expr)
and assign = Assign of (string * expr)

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr list * stmt
  | Return of expr

(* int x: name binding *)
type bind = 
    | typ * string
    | string * expr

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list
