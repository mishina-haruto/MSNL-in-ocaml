
type element =
    | Literal of string
    | Variable of string
    | Context of string
    | Wrapping of element
    | Multiset of element list
    | Sequence of element list

let rec matching data1 data2 theta = 
    let rec multisetMatching m1 m2 theta = 
        let rec subMultisetMatching prev x next theta = match next with
            [] -> (None, [])
            | first :: rest ->
                match matching first x theta with
                    None -> subMultisetMatching (prev @ first :: []) x rest theta
                    | Some (p) -> (Some (theta), prev @ rest)
        in
        match m2 with
        [] -> (match m1 with
            [] -> Some (theta)
            | first1 :: rest1 -> None
            )
        | first2 :: newM2 -> match first2 with
            | Context (c) -> matching (Multiset m1) first2 theta
            | _ -> (match subMultisetMatching [] first2 m1 theta with
                (None, []) -> None
                | (None, first :: rest) -> None
                | (Some (p), newM1) -> multisetMatching newM1 newM2 theta)
    in
    let rec sequenceMatching s1 s2 theta = 
        let rec subSequenceMatching s1prev s1next s2 theta = 
            let rec subSubSequenceMatching s1 s2 theta = match s2 with
                [] -> raise (Sys_error "subSubSequenceMatching")
                | first2 :: rest2 -> match s1 with
                    [] -> None(* Some (theta)かも *)
                    | first1 :: rest1 ->
                        match matching first1 first2 theta with
                            None -> None
                            | Some (p) -> sequenceMatching rest1 rest2 p
            in
            let result = subSubSequenceMatching ((Sequence s1prev) :: s1next) s2 theta in
            match result with
                None -> (match s1next with
                    [] -> None
                    | first :: rest -> 
                        subSequenceMatching (s1 @ first :: []) rest s2 theta)
                | Some (p) -> Some (p)
        in
        match s2 with
        [] -> 
            if s1 = [] then Some (theta)
            else None
        | first2 :: rest2 -> match first2 with
            | Context (c) -> 
                subSequenceMatching [] s1 s2 theta
            | _ -> match s1 with
                [] -> None
                | first1 :: rest1 ->
                    match matching first1 first2 theta with
                        None -> None
                        | Some (p) -> sequenceMatching rest1 rest2 p
    in
    match data1 with
    Literal (l1) -> (match data2 with
        Literal (l2) -> 
            if l1 = l2 then Some (theta)
            else None
        | Variable (v2) -> Some (theta)
        | Context (c2) -> Some (theta)
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> None)
    | Variable (v1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> Some (theta)
        | Context (c2) -> Some (theta)
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> None)
    | Context (c1) -> None
    | Wrapping (w1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> None
        | Context (c2) -> Some (theta)
        | Wrapping (w2) -> None
        | Multiset (m2) -> None
        | Sequence (s2) -> None)
    | Multiset (m1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> Some (theta)
        | Context (c2) -> Some (theta)
        | Wrapping (w2) -> None
        | Multiset (m2) -> multisetMatching m1 m2 theta
        | Sequence (s2) -> None)
    | Sequence (s1) -> (match data2 with
        Literal (l2) -> None
        | Variable (v2) -> Some (theta)
        | Context (c2) -> Some (theta)
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
    None -> false
    | Some (p) -> true

let test1 = testMatching (Literal "a") (Literal "a") = true
let test2 = testMatching (Literal "a") (Literal "b") = false
let test3 = testMatching (Literal "a") (Variable "a") = true
let test4 = testMatching (Literal "a") (Context "a") = true
let test5 = testMatching (Variable "a") (Variable "a") = true
let test6 = testMatching (Variable "a") (Context "a") = true
let test7 = testMatching (Wrapping (Literal "a")) (Context "a") = true
let test9 = testMatching (Wrapping (Literal "a")) (Wrapping (Literal "b")) = false
let test10 = testMatching (Multiset [(Literal "a")]) (Variable "a") = true
let test11 = testMatching (Multiset [(Literal "a")]) (Context "a") = true
let test12 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "b"); (Literal "a")]) = true
let test13 = testMatching (Multiset [(Literal "a"); (Literal "c")]) (Multiset [(Literal "b"); (Literal "a")]) = false
let test14 = testMatching (Multiset [(Literal "a")]) (Multiset [(Literal "b"); (Literal "a")]) = false
let test15 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "a")]) = false
let test16 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = true
let test17 = testMatching (Multiset [(Literal "a"); (Literal "c")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = false
let test18 = testMatching (Multiset [(Literal "a")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = false
let test19 = testMatching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "a"); (Context "a")]) = true
let test20 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "a")]) = true
let test21 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b")]) = false
let test22 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Literal "c")]) = true
let test23 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "b"); (Literal "c")]) = false
let test24 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a")]) = false
let test25 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Literal "c")]) = false
let test26 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "a"); (Context "a")]) = true
let test27 = testMatching (Sequence [(Literal "a")]) (Sequence [(Context "a"); (Literal "a")]) = true
let test28 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Context "a")]) = false
let test29 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Literal "c"); (Context "a")]) = true
let test30 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "b"); (Literal "c"); (Context "a")]) = false
let test31 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Context "a")]) = true
let test32 = testMatching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Context "a"); (Literal "a")]) = false
let test33 = testMatching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Literal "c"); (Context "a")]) = false
