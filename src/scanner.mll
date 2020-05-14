(* Ocamllex scanner for Meteor*)

{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
        (* WHITESPACE *)
         [' ' '\t' '\r' '\n'] { token lexbuf } 
        | "#"                  { comment lexbuf }
        (* TYPES *)
        | "int"         { INTTYPE }
        | "float"       { FLOATTYPE }
        | "bool"        { BOOLTYPE }
        | "mut"         { MUT }
        (* KEYWORDS *)
        | "let"         { LET }
        | "if"          { IF }
        | "else"        { ELSE }
        | "func"        { FUNC }
        | "while"       { WHILE }
        | "for"         { FOR }
        | "return"      { RETURN }
        (* BRACES *)
        | '['           { LBRACK }
        | ']'           { RBRACK }
        | '{'           { LBRACE }
        | '}'           { RBRACE }
        | '('           { LPAREN }
        | ')'           { RPAREN }
        (* SPECIAL SYMBOLS *)
        | ';'           { SEMI }
        | ','           { COMMA }
        | "::"          { COMP }
        | "->"          { ARROW }
        | '&'           { REF }
        (* NUMBER LITERALS *)
        | digit+ as lit              { INTLIT(int_of_string lit) }
        | (digit+ '.' digit+) as lit { FLOATLIT(float_of_string lit) }
        (* BOOLEAN LITERALS *)
        | "true"        { BLIT(true) }
        | "false"       { BLIT(false) }
        (* NUMBER OPERATORS *)
        | '+'           { PLUS }
        | '-'           { MINUS }
        | '*'           { TIMES }
        | '/'           { DIVIDE }
        | "+."          { FPLUS }
        | "-."          { FMINUS }
        | "*."          { FTIMES }
        | "/."          { FDIVIDE }
        (* BOOLEAN OPERATORS *)
        | '!'           { NOT }
        | "&&"          { AND }
        | "||"          { OR }
        (* COMPARISON OPERATORS *)
        | "=="          { EQ }
        | "==."         { FEQ }
        | "!="          { NEQ }
        | "!=."         { FNEQ }
        | '<'           { LT }
        | "<."          { FLT }
        | '>'           { GT }
        | ">."          { FGT }
        | "<="          { LEQ }
        | "<=."         { FLEQ }
        | ">="          { GEQ }
        | ">=."         { FGEQ }
        (* ASSIGN *)
        | '='           { ASSIGN }
        (* IDENTIFIERS *)
        | letter (digit | letter | '_' | '-')* as lem { VAR(lem) }
        | eof { EOF }
        | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
