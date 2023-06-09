
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

let calcBoolFactor boolFactor = match boolFactor with
    | Bool (bool) -> bool
    | IntEq (intvariable1, intvariable2) -> (match intvariable1 with
        | IntB (i1) -> (match intvariable2 with
            | IntB (i2) -> i1 = i2
            | VariableB (v2) -> false)
        | VariableB (v1) -> false)
    (* 途中 *)
    
let rec calcBoolExpr  boolExpr = match boolExpr with
    | And (boolExpr1, boolExpr2) -> (calcBoolExpr boolExpr1) && (calcBoolExpr boolExpr2)
    | Or (boolExpr1, boolExpr2) -> (calcBoolExpr boolExpr1) || (calcBoolExpr boolExpr2)
    | Factor (boolFactor) -> (calcBoolFactor boolFactor)
    | NegFactor (boolFactor) -> not (calcBoolFactor boolFactor)

let test00 = calcBoolFactor (Bool (false)) = false
let test01 = calcBoolFactor (IntEq ((IntB (1)), (IntB (1)))) = true
let test02 = calcBoolExpr (NegFactor (Bool (true))) = false
let test03 = calcBoolExpr (And ((Factor (Bool (true))), (Factor (Bool (false))))) = false