type literal_t = 
    | String of string
    | Int of int
    | Float of float
    | Bool of bool

type element_t =
    | Literal of literal_t
    | Variable of string
    | Context of string
    | Wrapping of element_t
    | Multiset of element_t list
    | Sequence of element_t list

type theta_t =
    | Replace of (element_t * element_t)
    | Equal of (element_t * element_t)

let rec getElementFromTheta theta element = match theta with
      [] -> None
    | first :: rest -> (match first with
          Replace (element1, element2) -> if element = element1 then Some (element2)
            else getElementFromTheta rest element
        | Equal (element1, element2) -> getElementFromTheta rest element)

let rec matching data1 data2 theta = 
    let rec multisetMatching m1 m2 theta = 
        let rec subMultisetMatching m1prev x m1next m2 theta = match m1next with
            [] -> None
            | first :: rest ->
                match matching first x theta with
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
            | Context (c) -> (match matching (Multiset m1) first2 theta with
                None -> None
                | Some (p) -> Some (p @ theta))
            | _ -> subMultisetMatching [] first2 m1 newM2 theta
    in
    let rec sequenceMatching s1 s2 theta = 
        let rec subSequenceMatching s1prev s1next s2 theta = 
            let rec subSubSequenceMatching s1 s2 theta = match s2 with
                [] -> raise (Sys_error "subSubSequenceMatching")
                | first2 :: rest2 -> match s1 with
                    [] -> raise (Sys_error "subSubSequenceMatching")
                    | first1 :: rest1 ->
                        match matching first1 first2 theta with
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
                    | first1 :: rest1 -> (match matching first1 first2 theta with
                        None -> None
                        | Some (p) -> sequenceMatching rest1 rest2 (p @ theta))))
    in
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
                    (* | IntExpr (e2) -> ... *)
                    | _ -> None)
                | Float (f1) -> (match l2 with
                      Float (f2) -> if f1 = f2 then Some ([])
                        else None
                    (* | FloatExpr (e2) -> ... *)
                    | _ -> None)
                | Bool (b1) -> (match l2 with
                      Bool (b2) -> if b1 = b2 then Some([])
                        else None
                    (* | BoolExpr (e2) -> ... *)
                    | _ -> None))
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

let rec permutation lst = 
    let rec subPermutation x lst = 
    let rec subSubPermutation prev x next = match next with
        [] -> (prev @ x :: []) :: []
        | first :: rest -> (prev @ x :: next) :: subSubPermutation (prev @ first :: []) x rest
    in
    match lst with
        [] -> []
        | first :: rest ->
            (subSubPermutation [] x first) @ (subPermutation x rest) 
    in
    match lst with
    [] -> [[]]
    | first :: rest -> 
        subPermutation first (permutation rest)

let testMatching data1 data2 = match matching data1 data2 [] with
    None -> [Equal (Literal (String ""), Literal (String ""))]
    | Some (p) -> p

(* a ? a *)
let test00 = testMatching (Literal (String "a")) (Literal (String "a")) = []
(* a ? b *)
let test01 = testMatching (Literal (String "a")) (Literal (String "b")) = [Equal (Literal (String ""), Literal (String ""))]
(* a ? @a *)
let test02 = testMatching (Literal (String "a")) (Variable "a") = [Replace ((Variable "a"), (Literal (String "a")))]
(* @a ? @a *)
let test03 = testMatching (Variable "a") (Variable "a") = [Replace ((Variable "a"), (Variable "a"))]
(* <a> ? <a> *)
let test04 = testMatching (Wrapping (Literal (String "a"))) (Wrapping (Literal (String "b"))) = [Equal (Literal (String ""), Literal (String ""))]
(* {a} ? @a *)
let test05 = testMatching (Multiset [(Literal (String "a"))]) (Variable "a") = [Replace ((Variable "a"), (Multiset [(Literal (String "a"))]))]
(* {a} ? $a *)
let test06 = testMatching (Multiset [(Literal (String "a"))]) (Context "a") = [Replace ((Context "a"), (Multiset [(Literal (String "a"))]))]
(* {a,b} ? {b,a} *)
let test07 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "b")); (Literal (String "a"))]) = []
(* {a,c} ? {b,a} *)
let test08 = testMatching (Multiset [(Literal (String "a")); (Literal (String "c"))]) (Multiset [(Literal (String "b")); (Literal (String "a"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* {a} ? {b,a} *)
let test09 = testMatching (Multiset [(Literal (String "a"))]) (Multiset [(Literal (String "b")); (Literal (String "a"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* {a,b} ? {a} *)
let test10 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "a"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* {a,b} ? {b,a,$a} *)
let test11 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Literal (String "b")); (Literal (String "a")); (Context "a")]) = [Replace ((Context "a"), Multiset [])]
(* {a,c} ? {b,a,$a} *)
let test12 = testMatching (Multiset [(Literal (String "a")); (Literal (String "c"))]) (Multiset [(Literal (String "b")); (Literal (String "a")); (Context "a")]) = [Equal (Literal (String ""), Literal (String ""))]
(* {a} ? {b,a,$a} *)
let test13 = testMatching (Multiset [(Literal (String "a"))]) (Multiset [(Literal (String "b")); (Literal (String "a")); (Context "a")]) = [Equal (Literal (String ""), Literal (String ""))]
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
let test19 = testMatching (Multiset [(Literal (String "a")); (Variable "b")]) (Multiset [(Variable "b"); (Literal (String "b"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* {a,b} ? $a *)
let test20 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Context "a")]) = [Replace ((Context "a"), Multiset [(Literal (String "a")); (Literal (String "b"))])]
(* {a,{b}} ? {{@b},a} *)
let test21 = testMatching (Multiset [(Literal (String "a")); (Multiset [(Literal (String "b"))])]) (Multiset [(Multiset [(Variable "b")]); (Variable "a")]) = [Replace (Variable "a", Literal (String "a")); Replace (Variable "b", Literal (String "b"))]
(* [a] ? [a] *)
let test22 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "a"))]) = []
(* [a] ? [b] *)
let test23 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:c] ? [a:c] *)
let test24 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a")); (Literal (String "c"))]) = []
(* [a:c] ? [b:c] *)
let test25 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "b")); (Literal (String "c"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:c] ? [a] *)
let test26 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a] ? [b:c] *)
let test27 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b")); (Literal (String "c"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a] ? [a:$a] *)
let test28 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "a")); (Context "a")]) = [Replace (Context "a", Sequence [])]
(* [a] ? [$a:a] *)
let test29 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Context "a"); (Literal (String "a"))]) = [Replace (Context "a", Sequence [])]
(* [a] ? [b:$a] *)
let test30 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b")); (Context "a")]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:c] ? [a:c:$a] *)
let test31 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a")); (Literal (String "c")); (Context "a")]) = [Replace (Context "a", Sequence [])]
(* [a:c] ? [b:c:$a] *)
let test32 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "b")); (Literal (String "c")); (Context "a")]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:c] ? [a:$a] *)
let test33 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Literal (String "a")); (Context "a")]) = [Replace (Context "a", Sequence [Literal (String "c")])]
(* [a:c] ? [$a:a] *)
let test34 = testMatching (Sequence [(Literal (String "a")); (Literal (String "c"))]) (Sequence [(Context "a"); (Literal (String "a"))]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a] ? [b:c:$a] *)
let test35 = testMatching (Sequence [(Literal (String "a"))]) (Sequence [(Literal (String "b")); (Literal (String "c")); (Context "a")]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:b] ? [@a:@b] *)
let test36 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b"))]) (Sequence [(Variable "a"); (Variable "b")]) = [Replace (Variable "b", Literal (String "b")); Replace (Variable "a", Literal (String "a"))]
(* [a:b:c] ? [@a:@b] *)
let test37 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b")); (Literal (String "c"))]) (Sequence [(Variable "a"); (Variable "b")]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:b] ? [@a:@b:@c] *)
let test38 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b"))]) (Sequence [(Variable "a"); (Variable "b"); (Variable "c")]) = [Equal (Literal (String ""), Literal (String ""))]
(* [a:b] ? [@a:$b:@c] *)
let test39 = testMatching (Sequence [(Literal (String "a")); (Literal (String "b"))]) (Sequence [(Variable "a"); (Context "b"); (Variable "c")]) = [Replace (Variable "c", Literal (String "b")); Replace (Context "b", Sequence []); Replace (Variable "a", Literal (String "a"))]
(* {a,b} ? {@a,@a} *)
let test40 = testMatching (Multiset [(Literal (String "a")); (Literal (String "b"))]) (Multiset [(Variable "a"); (Variable "a")]) = [Equal (Literal (String ""), Literal (String ""))]
(* {a,a} ? {@a,@a} *)
let test41 = testMatching (Multiset [(Literal (String "a")); (Literal (String "a"))]) (Multiset [(Variable "a"); (Variable "a")]) = [Replace (Variable "a", Literal (String "a"))]