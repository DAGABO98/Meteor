(* Abstract Syntax Tree and functions for printing it *)

type b_type = Int | Float | Bool 

type typ = 
    | Mut of b_type 
    | Ref of b_type
    | RType of b_type

type op = 
      Add | Sub | Mult | Div
    | FAdd | FSub | FMult | FDiv
    | And | Or | Not
    | Eq | FEq | Neq | FNeq | Lt | FLt | Gt | FGt
    | Leq | FLeq | Geq | FGeq 


type expr = 
    | IntLit of int | FloatLit of float | BoolLit of bool
    | Var of string
    | Binop of expr * op * expr
    | Call of string * expr list
    | Assign of string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr list * stmt
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | FAdd -> ".+"
  | FSub -> ".-"
  | FMult -> ".*"
  | FDiv -> "./"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | FEq -> ".=="
  | FNeq -> ".!="
  | FLt -> ".<"
  | FGt -> ".>"
  | FLeq -> ".<="
  | FGeq -> ".>="
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Var(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(es, s) -> "for (" ^ String.concat "" (List.map string_of_expr es) ^ ") " 
                       ^ string_of_stmt s

let string_of_typ = function
    Ref(x) ->  (match x with
            | Int -> "&int"
            | Float -> "&float"
            | Bool -> "&bool")
  | Mut(x) ->  (match x with
            | Int -> "mut &int"
            | Float -> "mut &float"
            | Bool -> "mut &bool")
  | RType(x) -> (match x with
            | Int -> "int"
            | Float -> "float"
            | Bool -> "bool")

let string_of_vdecl (t, id) = "let " ^ id ^ " -> " ^ string_of_typ t ^ ";\n"

let string_of_vinst (t, id) = id ^ " -> " ^ string_of_typ t

let string_of_fdecl fdecl =
  "func " ^ fdecl.fname ^ 
  "(" ^ String.concat ", " ((List.map string_of_vinst fdecl.formals)) 
  ^ ") -> " ^ string_of_typ fdecl.rtyp ^ " {\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
