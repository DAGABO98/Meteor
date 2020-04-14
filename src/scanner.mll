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
        | "char"        { CHARTYPE }
        | "float"       { FLOATTYPE }
        | "bool"        { BOOLTYPE }
        | "string"      { STRINGTYPE }
        | "foo"         { FOO }
        (* KEYWORDS *)
        | "let"         { LET }
        | "new"         { NEW }
        | "if"          { IF }
        | "else"        { ELSE }
        | "func"        { FUNC }
        | "mut"         { MUT }
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
        | '.'           { DOT }
        (* NUMBER LITERALS *)
        | digit+ as lit              { INTLIT(int_of_string lit) }
        | (digit+ '.' digit+) as lit { FLOATLIT(float_of_string lit) }
        (* BOOLEAN LITERALS *)
        | "true"        { BLIT(true) }
        | "false"       { BLIT(false) }
        (* CHARACTER LITERALS *)
        | "'"( _ as ch)"'"  { CHARLIT(ch) }
        (* STRING LITERALS *)
        (*| '"'( _ as str)'"' { STRLIT(str) }*)
        (* NUMBER OPERATORS *)
        | '+'           { PLUS }
        | '-'           { MINUS }
        | '*'           { TIMES }
        | '/'           { DIVIDE }
        | "**"          { POWER }
        | '%'           { MOD }
        | "+."          { FPLUS }
        | "-."          { FMINUS }
        | "*."          { FTIMES }
        | "/."          { FDIVIDE }
        | "**."         { FPOWER }
        (* BOOLEAN OPERATORS *)
        | '!'           { NOT }
        | "&&"          { AND }
        | "||"          { OR }
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
