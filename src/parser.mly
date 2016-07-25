%{ open Ast %}

%token FUNCTION
%token LPAREN RPAREN COMMA COLON LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ MODEQ
%token END RETURN IF ELIF ELSE FOR WHILE IN BREAK CONTINUE
%token INT BOOL FLOAT STRING NONE DATAFRAME

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOL EOF 

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left DOT 
%left OR
%left AND
%right PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ MODULUSEQ
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

/* var_decl list * func_decl list */
decls:
               { [], []}
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

/* Function declaration */
fdecl:
  FUNCTION typ ID LPAREN formals_opt RPAREN COLON EOL fbody END EOL
  { { 
    typ = $2;
    fname = $3;
    formals = $5;
    fbody = $9;
  } }

fbody:
    /* nothing */ { { f_vdecls = []; f_stmts = []; } }
  | fbody vdecl { { f_vdecls = $1.f_vdecls @ [$2]; f_stmts = $1.f_stmts; } }
  | fbody stmt { { f_vdecls = $1.f_vdecls; f_stmts = $1.f_stmts @ [$2]; } }

formals_opt:
    /* nothing */  { [] }
  | formal_list    { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT      { Int }
  | FLOAT    { Float }
  | BOOL     { Bool }
  | STRING   { String }
  | NONE     { None }
  | typ DATAFRAME  { Tuple($1, 0) }

/* Variable Declaration */
vdecl:
   typ ID ASSIGN expr EOL { match $1 with 
                              Tuple(ty, _) -> 
                                (match $4 with 
                                  TupleLit l -> (Tuple(ty, List.length l), $2, $4)
                                  | Call(_,_) -> (Tuple(ty, 0), $2, $4)
                                  | _ -> raise (Failure "You can only define a tuple with an expression with a tuple grammar.") )
                            | _ -> ($1, $2, $4)  } 

/* Statements */

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    EOL                                     { Nostmt }
  | expr EOL                                { Expr $1 }
  | BREAK EOL                               { Break }
  | CONTINUE EOL                            { Continue }
  | RETURN EOL                              { Return Noexpr }
  | RETURN expr EOL                         { Return $2 }
  | IF internal_if EOL                      { $2 }
  | vdecl                                   { Declaration($1) }
/* Before:
  If (Expr, Stmts to execute if yes, Stmts to execute if no)

Now:
  If (Expr, List of Stmts to execute if yes, [List of Elifs(expr, List of stmts to execute if yes)], List of Stmts to execute if no) */

  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN internal_block EOL
                                            { For($3, $5, $7, $9) }
  | FOR expr IN expr internal_block EOL     { ForIn($2, $4, $5) }
  | WHILE expr internal_block EOL           { While($2, $3) }
  | expr IN expr COLON EOL                  { In($1, $3)}

internal_block:
  | COLON EOL stmt_list END                 { Block(List.rev $3) }

internal_if:
  | expr COLON EOL stmt_list elif_list else_list END  
                                            { If($1, Block(List.rev $4), 
                                              Block(List.rev $5), Block(List.rev $6)) }

elif_list:
    /* nothing */  { [] }
  | elif elif_list { $1 :: $2 }

elif:
    ELIF expr COLON EOL stmt_list          { Elif($2, Block(List.rev $5)) }
 
else_list:
    /* nothing */      { [] }
  | ELSE COLON EOL stmt_list  { $4 }

/* Expressions */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL                   { IntLit($1) }
  | FLOAT_LITERAL                 { FloatLit($1) }
  | STRING_LITERAL                { StringLit($1) }  
  | TRUE                          { BoolLit(true) }
  | FALSE                         { BoolLit(false) }
  | ID                            { Id($1) }
  | expr PLUS    expr             { Binop($1, Add,   $3) }
  | expr MINUS   expr             { Binop($1, Sub,   $3) }
  | expr TIMES   expr             { Binop($1, Mult,  $3) }
  | expr DIVIDE  expr             { Binop($1, Div,   $3) }
  | expr MODULUS expr             { Binop($1, Mod,   $3) }
  | ID PLUSEQ    expr             { let id1 = Id($1) in
                                    Assign($1, Binop(id1, Add, $3)) }
  | ID MINUSEQ   expr             { let id1 = Id($1) in
                                    Assign($1, Binop(id1, Sub, $3)) }
  | ID TIMESEQ   expr             { let id1 = Id($1) in
                                    Assign($1, Binop(id1, Mult, $3)) }
  | ID DIVIDEEQ  expr             { let id1 = Id($1) in
                                    Assign($1, Binop(id1, Div, $3)) }
  | ID MODULUSEQ expr             { let id1 = Id($1) in
                                    Assign($1, Binop(id1, Mod, $3)) }
  | expr EQ      expr             { Binop($1, Equal, $3) }
  | expr NEQ     expr             { Binop($1, Neq,   $3) }
  | expr LT      expr             { Binop($1, Less,  $3) }
  | expr LEQ     expr             { Binop($1, Leq,   $3) }
  | expr GT      expr             { Binop($1, Greater, $3) }
  | expr GEQ     expr             { Binop($1, Geq,   $3) }
  | expr AND     expr             { Binop($1, And,   $3) }
  | expr OR      expr             { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG          { Unop(Neg, $2) }
  | NOT expr                      { Unop(Not, $2) }
  | ID ASSIGN expr                { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN  { Call($1, $3) }
  | LPAREN expr RPAREN               { $2 }
  | LPAREN actuals_list RPAREN       { TupleLit(List.rev $2) }
  | ID LBRACKET expr RBRACKET        { Element($1, $3) }  
  | NONE                             { Noexpr }

actuals_opt:
    /* nothing */ { [] }
  | expr          { [$1] }
  | actuals_list  { List.rev $1 }

actuals_list:
    actual_sublist COMMA expr { $3 :: $1 }
  | actual_sublist COMMA      { $1 }

actual_sublist:
    expr                      { [$1] }
  | actual_sublist COMMA expr { $3 :: $1 }
