open Syntax

(* ----- intExpr.ml ----- *)

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

(* ----- boolExpr.ml ----- *)

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

(* ----- element.ml ----- *)

(* ----- apply.ml ----- *)

type theta_t =
    | Replace of (element_t * element_t)
    | Equal of (literal_t * literal_t)

let rec getElementFromTheta theta element = match theta with
      [] -> None
    | first :: rest -> (match first with
          Replace (element1, element2) -> if element = element1 then Some (element2)
            else getElementFromTheta rest element
        | Equal (element1, element2) -> getElementFromTheta rest element)

let rec multisetSort prev next contexts = match next with
    | [] -> prev @ contexts
    | first :: rest -> (match first with
        | Context (c) -> multisetSort prev rest (contexts @ [first])
        | _ -> multisetSort (prev @ [first]) rest contexts)

let rec subMatching data1 data2 theta = 
    match data1 with
      Literal (l1) -> (match data2 with
          Literal (l2) -> 
            (match l1 with
                  String (s1) -> (match l2 with
                      String (s2) -> if s1 = s2 then Some ([])
                        else None
                    | _ -> None)
                | Int (i1) -> (match l2 with
                      Int (i2) -> if i1 = i2 then Some ([])
                        else None
                    | IntExpr (e2) -> Some ([Equal (l1, l2)])
                    | _ -> None)
                (* | Float (f1) -> (match l2 with
                      Float (f2) -> if f1 = f2 then Some ([])
                        else None
                    (* | FloatExpr (e2) -> ... *)
                    | _ -> None) *)
                (* | Bool (b1) -> (match l2 with
                      Bool (b2) -> if b1 = b2 then Some([])
                        else None
                    (* | BoolExpr (e2) -> ... *)
                    | _ -> None) *)
                | _ -> raise (Sys_error "subMatching"))
        | Variable (v2) -> (match getElementFromTheta theta (Variable (v2)) with
              Some (e) -> if (Literal (l1)) = e then Some ([])
                else None
            | None -> Some ([Replace ((Variable (v2)), (Literal (l1)))]))
        | _ -> None)
    | Variable (v1) -> (match data2 with
        Variable (v2) -> Some ([Replace ((Variable (v2)), (Variable (v1)))])
        | _ -> None)
    | Context (c1) -> None
    | Wrapping (w1) -> None
    | Multiset (m1) -> (match data2 with
        Variable (v2) -> Some ([Replace ((Variable (v2)), (Multiset (m1)))])
        | Context (c2) -> Some ([Replace ((Context (c2)), (Multiset (m1)))])
        | Multiset (m2) -> multisetMatching m1 m2 theta
        | _ -> None)
    | Sequence (s1) -> (match data2 with
        Variable (v2) -> Some ([Replace ((Variable (v2)), (Sequence (s1)))])
        | Context (c2) -> Some ([Replace ((Context (c2)), (Sequence (s1)))])
        | Sequence (s2) -> sequenceMatching s1 s2 theta
        | _ -> None)

and multisetMatching m1 m2 theta = 
    let rec subMultisetMatching m1prev x m1next m2 theta = match m1next with
        [] -> None
        | first :: rest ->
            match subMatching first x theta with
                None -> subMultisetMatching (m1prev @ first :: []) x rest m2 theta
                | Some (p) -> (match multisetMatching (m1prev @ rest) m2 (p @ theta) with
                    None -> subMultisetMatching (m1prev @ first :: []) x rest m2 theta
                    | Some (p) -> Some (p))
    in
    match m2 with
    [] -> (match m1 with
        [] -> Some (theta)
        | first1 :: rest1 -> None
        )
    | first2 :: newM2 -> match first2 with
        | Context (c) -> (match subMatching (Multiset m1) first2 theta with
            None -> None
            | Some (p) -> Some (p @ theta))
        | _ -> subMultisetMatching [] first2 m1 newM2 theta

and sequenceMatching s1 s2 theta = 
    let rec subSequenceMatching s1prev s1next s2 theta = 
        let rec subSubSequenceMatching s1 s2 theta = match s2 with
            [] -> raise (Sys_error "subSubSequenceMatching")
            | first2 :: rest2 -> match s1 with
                [] -> raise (Sys_error "subSubSequenceMatching")
                | first1 :: rest1 ->
                    match subMatching first1 first2 theta with
                        None -> None
                        | Some (p) -> sequenceMatching rest1 rest2 (p @ theta)
        in
        let result = subSubSequenceMatching ((Sequence s1prev) :: s1next) s2 theta in
        match result with
            None -> (match s1next with
                [] -> None
                | first :: rest -> 
                    subSequenceMatching (s1prev @ first :: []) rest s2 theta)
            | Some (p) -> Some (p)
    in
    match s2 with
        [] -> 
            if s1 = [] then Some (theta)
            else None
        | first2 :: rest2 -> (match first2 with
            Context (c) -> subSequenceMatching [] s1 s2 theta
            | _ -> (match s1 with
                [] -> None
                | first1 :: rest1 -> (match subMatching first1 first2 theta with
                    None -> None
                    | Some (p) -> sequenceMatching rest1 rest2 (p @ theta))))

let rec separateReplaceAndEqual theta replaces equals = match theta with
    | [] -> (replaces, equals)
    | first :: rest -> (match first with
        | Replace (e1, e2) -> separateReplaceAndEqual rest ((e1, e2) :: replaces) equals
        | Equal (l1, l2) -> separateReplaceAndEqual rest replaces ((l1, l2) :: equals))

let rec getElementFromReplaces replaces element = match replaces with
    | [] -> None
    | (e1, e2) :: rest ->
        if element = e1 then Some (e2)
        else getElementFromReplaces rest element

let rec applyReplacesToIntExpr intExpr replaces = match intExpr with
    | Plus (intexpr, intterm) -> Plus (applyReplacesToIntExpr intexpr replaces, applyReplacesToIntTerm intterm replaces)
    | Minus (intexpr, intterm) -> Minus (applyReplacesToIntExpr intexpr replaces, applyReplacesToIntTerm intterm replaces)
    | Term (term) -> Term (applyReplacesToIntTerm term replaces)

and applyReplacesToIntTerm intTerm replaces = match intTerm with
    | Times (intterm, intfactor) -> Times (applyReplacesToIntTerm intterm replaces, applyReplacesToIntFactor intfactor replaces)
    | Devide (intterm, intfactor) -> Devide (applyReplacesToIntTerm intterm replaces, applyReplacesToIntFactor intfactor replaces)
    | Factor (factor) -> Factor (applyReplacesToIntFactor factor replaces)

and applyReplacesToIntFactor intFactor replaces = match intFactor with
    | Expr (intexpr) -> Expr (applyReplacesToIntExpr intexpr replaces)
    | Int (int) -> Int (int)
    | Variable (v) -> 
        let e = getElementFromReplaces replaces (Variable (v)) in
        (match e with
            | Some (s) -> (match s with
                | Literal (l) -> (match l with
                    | Int (i) -> Int(i)
                    | _ -> intFactor)
                | _ -> intFactor)
            | None -> intFactor)

let applyReplacesToIntVariable intVariable replaces = match intVariable with
    | IntB (i) -> IntB (i)
    | VariableB (v) ->
        let e = getElementFromReplaces replaces (Variable (v)) in
        (match e with
            | Some (s) -> (match s with
                | Literal (l) -> (match l with
                    | Int (i) -> IntB (i)
                    | _ -> intVariable)
                | _ -> intVariable)
            | None -> intVariable)

let rec applyReplacesToBoolExpr boolExpr replaces = match boolExpr with
    | And (boolexpr1, boolexpr2) -> And (applyReplacesToBoolExpr boolexpr1 replaces, applyReplacesToBoolExpr boolexpr2 replaces)
    | Or (boolexpr1, boolexpr2) -> Or (applyReplacesToBoolExpr boolexpr1 replaces, applyReplacesToBoolExpr boolexpr2 replaces)
    | Factor (factor) -> Factor (applyReplacesToBoolFactor factor replaces)
    | NegFactor (factor) -> NegFactor (applyReplacesToBoolFactor factor replaces)

and applyReplacesToBoolFactor boolFactor replaces = match boolFactor with
    | Bool (bool) -> Bool (bool)
    | IntEq (intvariable1, intvariable2) -> IntEq (applyReplacesToIntVariable intvariable1 replaces, applyReplacesToIntVariable intvariable2 replaces)

let variableList = ref [""]

let rec subGetIndexFromVariableList variable list index = match list with
    | [] -> (variableList := !variableList @ variable :: []; string_of_int index)
    | first :: rest ->
        if variable = first then string_of_int index
        else subGetIndexFromVariableList variable rest (index+1)

let getIndexFromVariableList variable = subGetIndexFromVariableList variable !variableList (-1)

let rec getIndexFromVariableListForSet set = match set with
    | [] -> []
    | first :: rest -> (match first with
        | Variable (v) -> Variable(getIndexFromVariableList v) :: (getIndexFromVariableListForSet rest)
        | _ -> first :: (getIndexFromVariableListForSet rest))

let rec applyReplacesToElement element replaces = match element with
    | Literal (l) -> (match l with
        | IntExpr (i) -> 
            let intResult = calcIntExpr (applyReplacesToIntExpr i replaces) in
            (match intResult with
                | Some (s) -> Literal (Int (s))
                | None -> Literal (BoolExpr (Factor (Bool (false)))))
        (* | FloatExpr *)
        | BoolExpr (b) -> 
            let boolResult = calcBoolExpr (applyReplacesToBoolExpr b replaces) in
            Literal (BoolExpr (Factor (Bool (boolResult))))
        | _ -> element)
    | Variable (v) ->
        let e = getElementFromReplaces replaces element in
        (match e with
            | Some (s) -> (match s with
                | Variable (sv) -> Variable (getIndexFromVariableList sv)
                | _ -> s)
            | None -> Variable (getIndexFromVariableList v))
    | Context (c) -> raise (Sys_error "applyReplacesToElement")
    | Wrapping (w) -> Wrapping (applyReplacesToElement w replaces)
    | Multiset (m) -> Multiset (applyReplacesToSet m replaces)
    | Sequence (s) -> Sequence (applyReplacesToSet s replaces)

and applyReplacesToSet multiset replaces =
    let rec subApplyReplacesToSet before after replaces = match after with
        | [] -> before
        | first :: rest ->
            let e = getElementFromReplaces replaces first in
            (match first with
                | Context (c) -> (match e with
                    | Some (s) -> (match s with
                        | Multiset (m) -> subApplyReplacesToSet (before @ (getIndexFromVariableListForSet m)) rest replaces
                        | Sequence (q) -> subApplyReplacesToSet (before @ (getIndexFromVariableListForSet q)) rest replaces
                        | _ -> raise (Sys_error "applyReplacesToSet"))
                    | None -> raise (Sys_error "applyReplacesToSet"))
                | _ -> (match e with
                    | Some (s) -> subApplyReplacesToSet (before @ s :: []) rest replaces
                    | None -> subApplyReplacesToSet (before @ (applyReplacesToElement first replaces) :: []) rest replaces))
    in
    subApplyReplacesToSet [] multiset replaces

let rec checkEquals equals replaces = match equals with
    | [] -> true
    | (l1, l2) :: rest -> (match l2 with
        | IntExpr (intExpr) ->  (match (calcIntExpr (applyReplacesToIntExpr intExpr replaces)) with
            | Some (i) -> 
                if l1 = Int (i) then checkEquals rest replaces
                else false
            | None -> false)
        (* | floatExpr -> ... *)
        | BoolExpr (boolExpr) -> 
            if l1 = BoolExpr (Factor (Bool (calcBoolExpr (applyReplacesToBoolExpr boolExpr replaces)))) then checkEquals rest replaces
            else false
        | _ -> raise (Sys_error "checkEquals"))

let rec subDataMatching data1 data2 theta =
    let rec subSubDataMatching d1prev x d1next d2 theta = match d1next with
        | [] -> None
        | first :: rest ->
            match subMatching first x theta with
                | None -> subSubDataMatching (d1prev @ first :: []) x rest d2 theta
                | Some (p) -> (match subDataMatching (d1prev @ rest) d2 (p @ theta) with
                    | None -> subSubDataMatching (d1prev @ first :: []) x rest d2 theta
                    | Some (p) -> Some (p))
    in
    match data2 with
        | [] -> (match data1 with
            | [] -> 
                let (replaces, equals) = separateReplaceAndEqual theta [] [] in
                if checkEquals equals replaces then Some (replaces)
                else None
            | first1 :: rest1 -> None
            )
        | first2 :: newData2 -> (match first2 with
            | Context (c) -> (match subMatching (Multiset data1) first2 theta with
                | None -> None
                | Some (p) -> 
                    let (replaces, equals) = separateReplaceAndEqual (p @ theta) [] [] in
                    if checkEquals equals replaces then Some (replaces)
                    else None)
            | _ -> subSubDataMatching [] first2 data1 newData2 theta)

let rec changeGuardToTheta theta guard = match guard with
    | [] -> theta
    | first :: rest -> changeGuardToTheta (theta @ (Equal (BoolExpr (Factor (Bool (true))), first)) :: []) rest

let dataMatching data1 data2 guard = match data1 with
    | Multiset (m1) -> (match data2 with
        | Multiset (m2) -> subDataMatching (multisetSort [] m1 []) (multisetSort [] m2 []) ([Equal (BoolExpr (Factor (Bool (true))), guard)])
        | _ -> raise (Sys_error "subSubSequenceMatching"))
    | _ -> raise (Sys_error "subSubSequenceMatching")

let applyInstruction data head guard body =
    variableList := [""];
    let newHead = match head with
        | Multiset (h) -> Multiset ((Context "$") :: h)
        | _ -> raise (Sys_error "applyInstruction")
    in
    let newBody = match body with
        | Multiset (b) -> Multiset ((Context "$") :: b)
        | _ -> raise (Sys_error "applyInstruction")
    in
    let mutchingResult = dataMatching data newHead guard in
    match mutchingResult with
        | Some (replaces) -> (true, applyReplacesToElement newBody replaces)
        | None -> (false, data)

(* ----- tests ----- *)

let testMatching data1 data2 = match subMatching data1 data2 [] with
    None -> [Equal (String "", String "")]
    | Some (p) -> p

(* a ? a *)
let test00 = testMatching (Literal (String "a")) (Literal (String "a")) = []
(* a ? b *)
let test01 = testMatching (Literal (String "a")) (Literal (String "b")) = [Equal (String "", String "")]
(* a ? @a *)
let test02 = testMatching (Literal (String "a")) (Variable "a") = [Replace ((Variable "a"), (Literal (String "a")))]
(* @a ? @a *)
let test03 = testMatching (Variable "a") (Variable "a") = [Replace ((Variable "a"), (Variable "a"))]
(* <a> ? <a> *)
let test04 = testMatching (Wrapping (Literal (String "a"))) (Wrapping (Literal (String "b"))) = [Equal (String "", String "")]
(* {a} ? @a *)
let test05 = testMatching (Multiset [(Literal (String "a"))]) (Variable "a") = [Replace ((Variable "a"), (Multiset [(Literal (String "a"))]))]
(* {a} ? $a *)
let test06 = testMatching (Multiset [(Literal (String "a"))]) (Context "a") = [Replace ((Context "a"), (Multiset [(Literal (String "a"))]))]
(* {a,b} ? {b,a} *)
let test07 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "b")); (Literal (String "a"))]) = []
(* {a,c} ? {b,a} *)
let test08 = testMatching (Multiset [(Literal (String "a")); (Literal (String "c"))]) (Multiset [(Literal (String "b")); (Literal (String "a"))]) = [Equal (String "", String "")]
(* {a} ? {b,a} *)
let test09 = testMatching (Multiset [(Literal (String "a"))]) (Multiset [(Literal (String "b")); (Literal (String "a"))]) = [Equal (String "", String "")]
(* {a,b} ? {a} *)
let test10 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "a"))]) = [Equal (String "", String "")]
(* {a,b} ? {b,a,$a} *)
let test11 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "b")); (Literal (String "a")); (Context "a")]) = [Replace ((Context "a"), Multiset [])]
(* {a,c} ? {b,a,$a} *)
let test12 = testMatching (Multiset [(Literal (String "a")); (Literal (String "c"))]) (Multiset [(Literal (String "b")); (Literal (String "a")); (Context "a")]) = [Equal (String "", String "")]
(* {a} ? {b,a,$a} *)
let test13 = testMatching (Multiset [(Literal (String "a"))]) (Multiset [(Literal (String "b")); (Literal (String "a")); (Context "a")]) = [Equal (String "", String "")]
(* {a,b} ? {a,$a} *)
let test14 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "a")); (Context "a")]) = [Replace ((Context "a"), Multiset [(Literal (String "b"))])]
(* {a,b} ? {@a,@b} *)
let test15 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Variable "a"); (Variable "b")]) = [Replace (Variable "b", Literal (String "b")); Replace (Variable "a", Literal (String "a"))]
(* {a,@b} ? {@b,a} *)
let test16 = testMatching (Multiset [(Literal (String "a")); (Variable "b")]) (Multiset [(Literal (String "a")); (Variable "b")]) = [Replace (Variable "b", Variable "b")]
(* {@b,a} ? {a,@b} *)
let test17 = testMatching (Multiset [(Variable "b"); (Literal (String "a"))]) (Multiset [(Literal (String "a")); (Variable "b")]) = [Replace (Variable "b", Variable "b")]
(* {a,@b} ? {@b,a} *)
let test18 = testMatching (Multiset [(Literal (String "a")); (Variable "b")]) (Multiset [(Variable "b"); (Literal (String "a"))]) = [Replace (Variable "b", Variable "b")]
(* {a,@b} ? {@b,b} *)
let test19 = testMatching (Multiset [(Literal (String "a")); (Variable "b")]) (Multiset [(Variable "b"); (Literal (String "b"))]) = [Equal (String "", String "")]
(* {a,b} ? $a *)
let test20 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Context "a")]) = [Replace ((Context "a"), Multiset [(Literal (String "a")); (Literal (String "b"))])]
(* {a,{b}} ? {{@b},a} *)
let test21 = testMatching (Multiset [(Literal (String "a")); (Multiset [(Literal (String "b"))])]) (Multiset [(Multiset [(Variable "b")]); (Variable "a")]) = [Replace (Variable "a", Literal (String "a")); Replace (Variable "b", Literal (String "b"))]
(* [a] ? [a] *)
let test22 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "a"))]) = []
(* [a] ? [b] *)
let test23 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b"))]) = [Equal (String "", String "")]
(* [a:c] ? [a:c] *)
let test24 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a")); (Literal (String "c"))]) = []
(* [a:c] ? [b:c] *)
let test25 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "b")); (Literal (String "c"))]) = [Equal (String "", String "")]
(* [a:c] ? [a] *)
let test26 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a"))]) = [Equal (String "", String "")]
(* [a] ? [b:c] *)
let test27 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b")); (Literal (String "c"))]) = [Equal (String "", String "")]
(* [a] ? [a:$a] *)
let test28 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "a")); (Context "a")]) = [Replace (Context "a", Sequence [])]
(* [a] ? [$a:a] *)
let test29 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Context "a"); (Literal (String "a"))]) = [Replace (Context "a", Sequence [])]
(* [a] ? [b:$a] *)
let test30 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b")); (Context "a")]) = [Equal (String "", String "")]
(* [a:c] ? [a:c:$a] *)
let test31 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a")); (Literal (String "c")); (Context "a")]) = [Replace (Context "a", Sequence [])]
(* [a:c] ? [b:c:$a] *)
let test32 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "b")); (Literal (String "c")); (Context "a")]) = [Equal (String "", String "")]
(* [a:c] ? [a:$a] *)
let test33 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a")); (Context "a")]) = [Replace (Context "a", Sequence [Literal (String "c")])]
(* [a:c] ? [$a:a] *)
let test34 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Context "a"); (Literal (String "a"))]) = [Equal (String "", String "")]
(* [a] ? [b:c:$a] *)
let test35 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b")); (Literal (String "c")); (Context "a")]) = [Equal (String "", String "")]
(* [a:b] ? [@a:@b] *)
let test36 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b"))]) (Sequence [(Variable "a"); (Variable "b")]) = [Replace (Variable "b", Literal (String "b")); Replace (Variable "a", Literal (String "a"))]
(* [a:b:c] ? [@a:@b] *)
let test37 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b")); (Literal (String "c"))]) (Sequence [(Variable "a"); (Variable "b")]) = [Equal (String "", String "")]
(* [a:b] ? [@a:@b:@c] *)
let test38 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b"))]) (Sequence [(Variable "a"); (Variable "b"); (Variable "c")]) = [Equal (String "", String "")]
(* [a:b] ? [@a:$b:@c] *)
let test39 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b"))]) (Sequence [(Variable "a"); (Context "b"); (Variable "c")]) = [Replace (Variable "c", Literal (String "b")); Replace (Context "b", Sequence []); Replace (Variable "a", Literal (String "a"))]
(* {a,b} ? {@a,@a} *)
let test40 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Variable "a"); (Variable "a")]) = [Equal (String "", String "")]
(* {a,a} ? {@a,@a} *)
let test41 = testMatching (Multiset [(Literal (String "a")); (Literal (String "a"))]) (Multiset [(Variable "a"); (Variable "a")]) = [Replace (Variable "a", Literal (String "a"))]
(* 0 ? 2+1 *)
let test42 = dataMatching (Multiset [Literal (Int (3))]) (Multiset [Literal (IntExpr (Plus ((Term (Factor (Int (2)))), (Factor (Int (1))))))]) (BoolExpr (NegFactor (Bool (false)))) = Some ([])
(* a ? @b -> @b,@b *)
let test43 = applyInstruction (Multiset [(Literal (String ("a")))]) (Multiset [(Variable ("b")); (Context ("c"))]) (BoolExpr (Factor (Bool (true)))) (Multiset [(Variable ("b")); (Variable ("b"))]) = (true, Multiset [Literal (String "a"); Literal (String "a")])
