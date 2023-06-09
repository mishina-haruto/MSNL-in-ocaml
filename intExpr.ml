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

let rec calcIntExpr intExpr = match intExpr with
    | Plus (expr, term) -> (match (calcIntExpr expr) with
        | Some (int1) -> (match (calcIntTerm term) with
            | Some (int2) -> Some (int1 + int2)
            | None -> None)
        | None -> None)
    | Minus (expr, term) -> (match (calcIntExpr expr) with
        | Some (int1) -> (match (calcIntTerm term) with
            | Some (int2) -> Some (int1 - int2)
            | None -> None)
        | None -> None)
    | Term (term) -> calcIntTerm term

and calcIntTerm intTerm = match intTerm with
    | Times (term, factor) -> (match (calcIntTerm term) with
        | Some (int1) -> (match (calcIntFactor factor) with
            | Some (int2) -> Some (int1 * int2)
            | None -> None)
        | None -> None)
    | Devide (term, factor) -> (match (calcIntTerm term) with
        | Some (int1) -> (match (calcIntFactor factor) with
            | Some (int2) -> Some (int1 / int2)
            | None -> None)
        | None -> None)
    | Factor (factor) -> calcIntFactor factor

and calcIntFactor intFactor = match intFactor with
    | Expr (expr) -> calcIntExpr expr
    | Int (int) -> Some (int)
    | Variable (variable) -> None

let test00 = calcIntFactor (Int (0)) = Some (0)
let test01 = calcIntFactor (Variable ("a")) = None
let test02 = calcIntTerm (Times ((Factor (Int (2))), (Int (3)))) = Some (6)
let test03 = calcIntTerm (Devide ((Factor (Int (6))), (Int (3)))) = Some (2)
let test04 = calcIntTerm (Devide ((Factor (Variable ("a"))), (Int (3)))) = None
let test05 = calcIntExpr (Plus ((Term (Factor (Int (2)))), (Factor (Int (1))))) = Some (3)