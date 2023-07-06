type intExpr_t =
  | Plus of (intExpr_t * intTerm_t)
  | Minus of (intExpr_t * intTerm_t)
  | Term of intTerm_t

and intTerm_t = 
  | Times of (intTerm_t * intFactor_t)
  | Devide of (intTerm_t * intFactor_t)
  | Factor of intFactor_t

and intFactor_t = 
  | Expr of intExpr_t
  | Int of int
  | Variable of string

type intVariabel_t = 
  | IntB of (int)
  | VariableB of (string)

type boolExpr_t =
  | And of (boolExpr_t * boolExpr_t)
  | Or of (boolExpr_t * boolExpr_t)
  | Factor of (boolFactor_t)
  | NegFactor of (boolFactor_t)

and boolFactor_t =
  | Bool of (bool)
  | IntEq of (intVariabel_t * intVariabel_t)
  (* 途中 *)

type literal_t = 
  | String of string
  | Int of int
  (* | Float of float *)
  | IntExpr of intExpr_t
  (* | FloatExpr of floatExpr_t *)
  | BoolExpr of boolExpr_t

type element_t =
  | Literal of literal_t
  | Variable of string
  | Context of string
  | Wrapping of element_t
  | Multiset of element_t list
  | Sequence of element_t list

type instruction_t =
  | Inst of (element_t * literal_t * element_t)