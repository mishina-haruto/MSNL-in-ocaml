type element =
    | Literal of string
    | Variable of string
    | Context of string
    | Wrapping of element
    | Multiset of element list
    | Sequence of element list

let rec matching data1 data2 = 
    let rec multisetMatching m1 m2 = 
        let rec subMultisetMatching prev x next = match next with
            [] -> raise Not_found
            | first :: rest ->
                if matching first x then prev @ rest
                else subMultisetMatching (prev @ first :: []) x rest
        in
        match m2 with
        [] -> (match m1 with
            [] -> true
            | first1 :: [] -> (match first1 with
                | Context (c) -> true
                | _ -> false)
            | _ -> false
            )
        | first2 :: newM2 -> match first2 with
            | Context (c) -> matching (Multiset m1) first2
            | _ -> (try multisetMatching (subMultisetMatching [] first2 m1) newM2 with
                | Not_found -> false
                | _ -> multisetMatching (subMultisetMatching [] first2 m1) newM2)
    in
    let rec sequenceMatching s1 s2 = 
        let rec subSequenceMatching s1prev s1next s2 = 
            let rec subSubSequenceMatching s1 s2 = match s2 with
                [] -> raise (Sys_error "subSubSequenceMatching")
                | first2 :: rest2 -> match s1 with
                    [] -> false
                    | first1 :: rest1 ->
                        if matching first1 first2 then sequenceMatching rest1 rest2
                        else false
            in
            let result = subSubSequenceMatching ((Multiset s1prev) :: s1next) s2  in
            if result then true
            else match s1next with
                [] -> false
                | first :: rest ->
                    subSequenceMatching (s1 @ first :: []) rest s2 
        in
        match s2 with
        [] -> 
            if s1 = [] then true
            else false
        | first2 :: rest2 -> match first2 with
            | Context (c) -> 
                subSequenceMatching [] s1 s2
            | _ -> match s1 with
                [] -> false
                | first1 :: rest1 ->
                    if matching first1 first2 then sequenceMatching rest1 rest2
                    else false
    in
    match data1 with
    Literal (l1) -> (match data2 with
        Literal (l2) -> 
            if l1 = l2 then true
            else false
        | Variable (v2) -> true
        | Context (c2) -> true
        | Wrapping (w2) -> false
        | Multiset (m2) -> false
        | Sequence (s2) -> false)
    | Variable (v1) -> (match data2 with
        Literal (l2) -> false
        | Variable (v2) -> true
        | Context (c2) -> true
        | Wrapping (w2) -> false
        | Multiset (m2) -> false
        | Sequence (s2) -> false)
    | Context (c1) -> false
    | Wrapping (w1) -> (match data2 with
        Literal (l2) -> false
        | Variable (v2) -> false
        | Context (c2) -> true
        | Wrapping (w2) -> false
        | Multiset (m2) -> false
        | Sequence (s2) -> false)
    | Multiset (m1) -> (match data2 with
        Literal (l2) -> false
        | Variable (v2) -> true
        | Context (c2) -> true
        | Wrapping (w2) -> false
        | Multiset (m2) -> multisetMatching m1 m2
        | Sequence (s2) -> false)
    | Sequence (s1) -> (match data2 with
        Literal (l2) -> false
        | Variable (v2) -> true
        | Context (c2) -> true
        | Wrapping (w2) -> false
        | Multiset (m2) -> false
        | Sequence (s2) -> sequenceMatching s1 s2)

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

let test1 = matching (Literal "a") (Literal "a") = true
let test2 = matching (Literal "a") (Literal "b") = false
let test3 = matching (Literal "a") (Variable "a") = true
let test4 = matching (Literal "a") (Context "a") = true
let test5 = matching (Variable "a") (Variable "a") = true
let test6 = matching (Variable "a") (Context "a") = true
let test7 = matching (Wrapping (Literal "a")) (Context "a") = true
let test9 = matching (Wrapping (Literal "a")) (Wrapping (Literal "b")) = false
let test10 = matching (Multiset [(Literal "a")]) (Variable "a") = true
let test11 = matching (Multiset [(Literal "a")]) (Context "a") = true
let test12 = matching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "b"); (Literal "a")]) = true
let test13 = matching (Multiset [(Literal "a"); (Literal "c")]) (Multiset [(Literal "b"); (Literal "a")]) = false
let test14 = matching (Multiset [(Literal "a")]) (Multiset [(Literal "b"); (Literal "a")]) = false
let test15 = matching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "a")]) = false
let test16 = matching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = true
let test17 = matching (Multiset [(Literal "a"); (Literal "c")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = false
let test18 = matching (Multiset [(Literal "a")]) (Multiset [(Literal "b"); (Literal "a"); (Context "a")]) = false
let test19 = matching (Multiset [(Literal "a"); (Literal "b")]) (Multiset [(Literal "a"); (Context "a")]) = true
let test20 = matching (Sequence [(Literal "a")]) (Sequence [(Literal "a")]) = true
let test21 = matching (Sequence [(Literal "a")]) (Sequence [(Literal "b")]) = false
let test22 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Literal "c")]) = true
let test23 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "b"); (Literal "c")]) = false
let test24 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a")]) = false
let test25 = matching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Literal "c")]) = false
let test26 = matching (Sequence [(Literal "a")]) (Sequence [(Literal "a"); (Context "a")]) = true
let test27 = matching (Sequence [(Literal "a")]) (Sequence [(Context "a"); (Literal "a")]) = true
let test28 = matching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Context "a")]) = false
let test29 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Literal "c"); (Context "a")]) = true
let test30 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "b"); (Literal "c"); (Context "a")]) = false
let test31 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Literal "a"); (Context "a")]) = true
let test32 = matching (Sequence [(Literal "a"); (Literal "c")]) (Sequence [(Context "a"); (Literal "a")]) = false
let test33 = matching (Sequence [(Literal "a")]) (Sequence [(Literal "b"); (Literal "c"); (Context "a")]) = false
