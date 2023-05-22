type intExpr_t =
    | Plus of (intExpr_t * intTerm_t)
    | Minus of (intExpr_t * intTerm_t)
    | Term of intTerm_t

type intTerm_t = 
    | Times of (intTerm_t * intFactor_t)
    | Devide of (intTerm_t * intFactor_t)
    | Factor of intFactor_t

type intFactor_t = 
    | Expr of intExpr_t
    | Int of int