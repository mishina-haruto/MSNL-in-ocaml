
type element =
    | Literal of string
    | Variable of string
    | Context of string
    | Wrapping of element
    | Multiset of element list
    | Sequence of element list

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
            if l1 = l2 then Some ([])
            else None
        | Variable (v2) -> Some ([((Variable (v2)), (Literal (l1)))])
        | Context (c2) -> None
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> None)
    | Variable (v1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> Some ([((Variable (v2)), (Variable (v1)))])
        | Context (c2) -> None
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> None)
    | Context (c1) -> None
    | Wrapping (w1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> None
        | Context (c2) -> None
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> None)
    | Multiset (m1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> Some ([((Variable (v2)), (Multiset (m1)))])
        | Context (c2) -> Some ([(Context (c2)), (Multiset (m1))])
        | Wrapping (w2) -> None
        | Multiset (m2) -> multisetMatching m1 m2 theta
        | Sequence (s2) -> None)
    | Sequence (s1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> Some ([((Variable (v2)), (Sequence (s1)))])
        | Context (c2) -> Some ([(Context (c2)), (Sequence (s1))])
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> sequenceMatching s1 s2 theta)

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
    None -> [((Literal ""), (Literal ""))]
    | Some (p) -> p

(* a ? a *)
let test00 = testMatching (Literal "a") (Literal "a") = []
(* a ? b *)
let test01 = testMatching (Literal "a") (Literal "b") = [((Literal ""), (Literal ""))]
(* a ? @a *)
let test02 = testMatching (Literal "a") (Variable "a") = [((Variable "a"), (Literal "a"))]
(* @a ? @a *)
let test03 = testMatching (Variable "a") (Variable "a") = [((Variable "a"), (Variable "a"))]
(* <a> ? <a> *)
let test04 = testMatching (Wrapping (Literal "a")) (Wrapping (Literal "b")) = [((Literal ""), (Literal ""))]
(* {a} ? @a *)
let test05 = testMatching (Multiset [(Literal "a")]) (Variable "a") = [((Variable "a"), (Multiset [(Literal "a")]))]
(* {a} ? $a *)
let test06 = testMatching (Multiset [(Literal "a")]) (Context "a") = [((Context "a"), (Multiset [(Literal "a")]))]
(* {a,b} ? {b,a} *)
let test07 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "b"); (Literal "a")]) = []
(* {a,c} ? {b,a} *)
let test08 = testMatching (Multiset [(Literal "a"); (Literal "c")]) (Multiset [(Literal "b"); (Literal "a")]) = [((Literal ""), (Literal ""))]
(* {a} ? {b,a} *)
let test09 = testMatching (Multiset [(Literal "a")]) (Multiset [(Literal "b"); (Literal "a")]) = [((Literal ""), (Literal ""))]
(* {a,b} ? {a} *)
let test10 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "a")]) = [((Literal ""), (Literal ""))]
(* {a,b} ? {b,a,$a} *)
let test11 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = [((Context "a"), Multiset [])]
(* {a,c} ? {b,a,$a} *)
let test12 = testMatching (Multiset [(Literal "a"); (Literal "c")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = [((Literal ""), (Literal ""))]
(* {a} ? {b,a,$a} *)
let test13 = testMatching (Multiset [(Literal "a")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = [((Literal ""), (Literal ""))]
(* {a,b} ? {a,$a} *)
let test14 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "a"); (Context "a")]) = [((Context "a"), Multiset [(Literal "b")])]
(* {a,b} ? {@a,@b} *)
let test15 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Variable "a"); (Variable "b")]) = [(Variable "b", Literal "b"); (Variable "a", Literal "a")]
(* {a,@b} ? {@b,a} *)
let test16 = testMatching (Multiset [(Literal "a"); (Variable "b")]) (Multiset [(Literal "a"); (Variable "b")]) = [(Variable "b", Variable "b")]
(* {@b,a} ? {a,@b} *)
let test17 = testMatching (Multiset [(Variable "b"); (Literal "a")]) (Multiset [(Literal "a"); (Variable "b")]) = [(Variable "b", Variable "b")]
(* {a,@b} ? {@b,a} *)
let test18 = testMatching (Multiset [(Literal "a"); (Variable "b")]) (Multiset [(Variable "b"); (Literal "a")]) = [(Variable "b", Variable "b")]
(* {a,@b} ? {@b,b} *)
let test19 = testMatching (Multiset [(Literal "a"); (Variable "b")]) (Multiset [(Variable "b"); (Literal "b")]) = [((Literal ""), (Literal ""))]
(* {a,b} ? $a *)
let test20 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Context "a")]) = [((Context "a"), Multiset [(Literal "a"); (Literal "b")])]
(* {a,{b}} ? {{@b},a} *)
let test21 = testMatching (Multiset [(Literal "a"); (Multiset [(Literal "b")])]) (Multiset [(Multiset [(Variable "b")]); (Variable "a")]) = [(Variable "a", Literal "a"); (Variable "b", Literal "b")]
(* [a] ? [a] *)
let test22 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "a")]) = []
(* [a] ? [b] *)
let test23 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b")]) = [((Literal ""), (Literal ""))]
(* [a:c] ? [a:c] *)
let test24 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Literal "c")]) = []
(* [a:c] ? [b:c] *)
let test25 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "b"); (Literal "c")]) = [((Literal ""), (Literal ""))]
(* [a:c] ? [a] *)
let test26 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a")]) = [((Literal ""), (Literal ""))]
(* [a] ? [b:c] *)
let test27 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Literal "c")]) = [((Literal ""), (Literal ""))]
(* [a] ? [a:$a] *)
let test28 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "a"); (Context "a")]) = [(Context "a", Sequence [])]
(* [a] ? [$a:a] *)
let test29 = testMatching (Sequence [(Literal "a")]) (Sequence [(Context "a"); (Literal "a")]) = [(Context "a", Sequence [])]
(* [a] ? [b:$a] *)
let test30 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Context "a")]) = [((Literal ""), (Literal ""))]
(* [a:c] ? [a:c:$a] *)
let test31 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Literal "c"); (Context "a")]) = [(Context "a", Sequence [])]
(* [a:c] ? [b:c:$a] *)
let test32 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "b"); (Literal "c"); (Context "a")]) = [((Literal ""), (Literal ""))]
(* [a:c] ? [a:$a] *)
let test33 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Context "a")]) = [(Context "a", Sequence [Literal "c"])]
(* [a:c] ? [$a:a] *)
let test34 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Context "a"); (Literal "a")]) = [((Literal ""), (Literal ""))]
(* [a] ? [b:c:$a] *)
let test35 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Literal "c"); (Context "a")]) = [((Literal ""), (Literal ""))]
(* [a:b] ? [@a:@b] *)
let test36 = testMatching (Sequence [(Literal "a"); (Literal "b")]) (Sequence [(Variable "a"); (Variable "b")]) = [(Variable "b", Literal "b"); (Variable "a", Literal "a")]
(* [a:b:c] ? [@a:@b] *)
let test37 = testMatching (Sequence [(Literal "a"); (Literal "b"); (Literal "c")]) (Sequence [(Variable "a"); (Variable "b")]) = [((Literal ""), (Literal ""))]
(* [a:b] ? [@a:@b:@c] *)
let test38 = testMatching (Sequence [(Literal "a"); (Literal "b")]) (Sequence [(Variable "a"); (Variable "b"); (Variable "c")]) = [((Literal ""), (Literal ""))]
(* [a:b] ? [@a:$b:@c] *)
let test38 = testMatching (Sequence [(Literal "a"); (Literal "b")]) (Sequence [(Variable "a"); (Context "b"); (Variable "c")]) = [(Variable "c", Literal "b"); (Context "b", Sequence []); (Variable "a", Literal "a")]
