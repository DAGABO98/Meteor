/* Ocamlyacc parser for Meteor */

%{
open Ast
%}

%token SEMI LSBRACE RSBRACE LBRACE RBRACE LPAREN RPAREN ASSIGN
%token PLUS MINUS TIMES DIVIDE POWER MOD FPLUS FMINUS FTIMES FDIVIDE FPOWER
%token NOT AND OR EQ FEQ NEQ FNEQ LT FLT GT FGT LEQ FLEQ GEQ FGEQ
%token EOF LET NEW IF ELSE FUNC MUT WHILE FOR PRINT RETURN
%token INTTYPE CHARTYPE FLOATTYPE BOOLTYPE STRINGTYPE
%token COMMA COMP ARROW REF DOT

%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BLIT
%token <char> CHARLIT
%token <string> STRLIT
%token <string> VAR

%start program
%type <Ast.program> program

%right ASSIGN
%left OR AND NOT
%left EQ FEQ NEQ FNEQ LT FLT GT FGT LEQ FLEQ GEQ FGEQ
%right ARROW COMP DOT
%left ELSE
%right MUT
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIVIDE MOD FTIMES FDIVIDE
%left POWER FPOWER

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ VAR { ($1, $2) }

typ:
    | INTTYPE       { Int }
    | CHARTYPE      { Char }
    | FLOATTYPE     { Float }
    | BOOLTYPE      { Bool }
    | STRINGTYPE    { String }
    | VAR           { Tvar($1) }
    | typ ARROW typ { Tarrow($1,$3) }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
  /* LITERALS */
    INTLIT           { IntLit($1)             }
  | FLOATLIT         { FloatLit($1)           }
  | BLIT             { BoolLit($1)            }
  | CHARLIT          { CharLit($1)            }
  | STRLIT           { StrLit($1)             }
  | VAR              { Var($1)                 }
  /* BOOLEAN OPERATIONS */
  | expr AND expr    { Binop($1, And, $3) }
  | expr OR expr     { Binop($1, Or, $3) }
  | expr EQ expr     { Binop($1, Eq, $3) }
  | expr EQ expr     { Binop($1, Eq, $3) }
  | expr FEQ expr    { Binop($1, FEq, $3) }
  | expr NEQ expr    { Binop($1, Neq, $3) }
  | expr FNEQ expr   { Binop($1, FNeq, $3) }
  | expr LT expr     { Binop($1, Lt, $3) }
  | expr FLT expr    { Binop($1, FLt, $3) }
  | expr GT expr     { Binop($1, Gt, $3) }
  | expr FGT expr    { Binop($1, FGt, $3) }
  | expr LEQ expr    { Binop($1, Leq, $3) }
  | expr FLEQ expr   { Binop($1, FLeq, $3) }
  | expr GEQ expr    { Binop($1, Geq, $3) }
  | expr FGEQ expr   { Binop($1, FGeq, $3) }
  /* MATH OPERATIONS */
  | expr PLUS   expr   { Binop($1, Add,   $3)   }
  | expr MINUS  expr   { Binop($1, Sub,   $3)   }
  | expr TIMES  expr   { Binop($1, Mult,   $3)   }
  | expr DIVIDE  expr  { Binop($1, Div,   $3)   }
  | expr POWER  expr   { Binop($1, Pow,   $3)   }
  | expr MOD  expr     { Binop($1, Mod,   $3)   }
  | expr MINUS  expr   { Binop($1, Sub,   $3)   }
  | expr FPLUS   expr  { Binop($1, FAdd,   $3)   }
  | expr FMINUS  expr  { Binop($1, FSub,   $3)   }
  | expr FTIMES  expr  { Binop($1, FMult,   $3)   }
  | expr FDIVIDE  expr { Binop($1, FDiv,   $3)   }
  | expr FPOWER  expr  { Binop($1, FPow,   $3)   }


  | VAR ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | VAR LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 } 
