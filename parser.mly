%{
  open Syntax
%}

%token <string> LITERALSTR
%token <int> LITERALINT
%token LITERALTRUE LITERALFALSE

%token <string> VARIABLE

%token <string> CONTEXT

%token LPAREN
%token RPAREN
%token LCBRACKET
%token RCBRACKET
%token LBRACKET
%token RBRACKET
%token LT
%token GT
%token COMMA
%token COLLON

%token PLUS
%token MINUS
%token TIMES
%token DEVIDE

%token AND
%token OR
%token NOT

%token GUARD
%token ARROW

%left AND OR

%token EQUAL

%token EOL

%start instruction_eol         /* the entry point */
%type <Syntax.instruction_t> instruction_eol

%%

instruction_eol:
  | instruction EOL           {$1}
;
instruction:
  | m_list GUARD bool_expr ARROW m_list { Inst (Multiset ($1), BoolExpr ($3),  Multiset ($5)) }
  | GUARD bool_expr ARROW m_list        { Inst (Multiset ([]), BoolExpr ($2),  Multiset ($4)) }
  | m_list GUARD bool_expr ARROW        { Inst (Multiset ($1), BoolExpr ($3),  Multiset ([])) }
  | GUARD bool_expr ARROW               { Inst (Multiset ([]), BoolExpr ($2),  Multiset ([])) }
  | m_list ARROW m_list                 { Inst (Multiset ($1), BoolExpr (Factor (Bool (true))), Multiset ($3)) }
  | ARROW m_list                        { Inst (Multiset ([]), BoolExpr (Factor (Bool (true))), Multiset ($2)) }
  | m_list ARROW                        { Inst (Multiset ($1), BoolExpr (Factor (Bool (true))), Multiset ([])) }
  | ARROW                               { Inst (Multiset ([]), BoolExpr (Factor (Bool (true))), Multiset ([])) }
  | m_list                              { Inst (Multiset ([]), BoolExpr (Factor (Bool (true))), Multiset ($1)) }
;
element:
  | literal                   { Literal ($1) }
  | VARIABLE                  { Variable ($1) }
  | CONTEXT                   { Context ($1) }
  | LT element GT             { Wrapping ($2)}
  | LCBRACKET RCBRACKET       { Multiset ([])}
  | LCBRACKET m_list RCBRACKET { Multiset ($2)}
  | LBRACKET RBRACKET         { Sequence ([]) }
  | LBRACKET s_list RBRACKET  { Sequence ($2) }
  /* | s_list                    { $1 } 優先度の付け方が分からない */
;
m_list:
  | element                   {[$1]}
  | m_list COMMA element      {$1 @ [$3]}
;
s_list:
  | element                   {[$1]}
  | s_list COLLON element     {$1 @ [$3]}
;
literal:
  | LITERALSTR                { String ($1) }
  | LITERALINT                { Int ($1) }
  | int_expr                  { IntExpr ($1) }
  | bool_expr                 { BoolExpr ($1) }
;
int_expr:
  | int_expr PLUS int_term    { Plus ($1, $3) }
  | int_expr MINUS int_term   { Minus ($1, $3) }
  | int_term                  { Term ($1) }
;
int_term:
  | int_term TIMES int_factor { Times ($1, $3) }
  | int_term DEVIDE int_factor { Devide ($1, $3) }
  | int_factor                { Factor ($1) }
;
int_factor:
  | LPAREN int_expr RPAREN    { Expr ($2) }
  | LITERALINT                { Int ($1) }
  | VARIABLE                  { Variable ($1) }
;
bool_expr:
  | bool_expr AND bool_expr   { And ($1, $3) }
  | bool_expr OR bool_expr    { Or ($1, $3) }
  | LPAREN bool_expr RPAREN   { $2 }
  | bool_factor               { Factor ($1) }
  | NOT bool_factor           { NegFactor ($2) }
;
bool_factor:
  | LITERALTRUE               { Bool (true) }
  | LITERALFALSE              { Bool (false) }
  | int_variable EQUAL int_variable {IntEq ($1, $3)}
;
int_variable:
  | LITERALINT                { IntB ($1)}
  | VARIABLE                  { VariableB ($1)}
;