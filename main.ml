(* File calc.ml *)

open Syntax

(* --- text_of_??? --- *)
let rec text_of_intexpr intexpr = match intexpr with
  | Plus (ie, it) -> "Plus (" ^ (text_of_intexpr ie) ^ ", " ^ (text_of_intterm it) ^ ")"
  | Minus (ie, it) -> "Minus (" ^ (text_of_intexpr ie) ^ ", " ^ (text_of_intterm it) ^ ")"
  | Term (it) -> "Term (" ^ (text_of_intterm it) ^ ")"

and text_of_intterm intterm = match intterm with
  | Times (it, ic) -> "Times (" ^ (text_of_intterm it) ^ ", " ^ (text_of_intfactor ic)
  | Devide (it, ic) -> "Devide (" ^ (text_of_intterm it) ^ ", " ^ (text_of_intfactor ic)
  | Factor (ic) -> "Factor (" ^ (text_of_intfactor ic) ^ ")"

and text_of_intfactor intfactor = match intfactor with
  | Expr (ie) -> "Expr (" ^ (text_of_intexpr ie) ^ ")"
  | Int (i) -> "Int (" ^ (string_of_int i) ^ ")"
  | Variable (v) -> "Variable (" ^ v ^ ")"

let text_of_intvariable intvariable = match intvariable with
  | IntB (i) -> "IntB (" ^ (string_of_int i) ^ ")"
  | VariableB (v) -> "VariableB (" ^ v ^ ")"

let rec text_of_boolexpr boolexpr = match boolexpr with
  | And (be1, be2) -> "And (" ^ (text_of_boolexpr be1) ^ ", " ^ (text_of_boolexpr be2) ^ ")"
  | Or (be1, be2) -> "Or (" ^ (text_of_boolexpr be1) ^ ", " ^ (text_of_boolexpr be2) ^ ")"
  | Factor (bf) -> "Factor (" ^ (text_of_boolfactor bf) ^ ")"
  | NegFactor (bf) -> "NegFactor (" ^ (text_of_boolfactor bf) ^ ")"

and text_of_boolfactor boolfactor = match boolfactor with
  | Bool (b) -> "Bool (" ^ (string_of_bool b) ^ ")"
  | IntEq (iv1, iv2) -> "IntEq (" ^ (text_of_intvariable iv1) ^ ", " ^ (text_of_intvariable iv2) ^ ")"

let text_of_literal literal = match literal with
  | String (s) -> "String (" ^ s ^ ")"
  | Int (i) -> "Int (" ^ (string_of_int i) ^ ")"
  | IntExpr (ie) -> "IntExpr (" ^ (text_of_intexpr ie) ^ ")"
  | BoolExpr (be) -> "BoolExpr (" ^ (text_of_boolexpr be) ^ ")"

let rec text_of_element element = match element with
  | Literal (l) -> "Literal (" ^ (text_of_literal l) ^ ")"
  | Variable (v) -> "Variable (" ^ v ^ ")"
  | Context (c) -> "Context (" ^ c ^ ")"
  | Wrapping (w) -> "Wrapping (" ^ (text_of_element w) ^ ")"
  | Multiset (m) -> "Multiset ([" ^ (text_of_elementlist m) ^ "])"
  | Sequence (s) -> "Sequence ([" ^ (text_of_elementlist s) ^ "])"

and text_of_elementlist list = match list with
  | [] -> ""
  | first :: [] -> (text_of_element first)
  | first :: rest -> (text_of_element first) ^ "; " ^ (text_of_elementlist rest)

(* --- string_of_??? --- *)

let rec string_of_intexpr intexpr = match intexpr with
  | Plus (ie, it) -> (string_of_intexpr ie) ^ "+" ^ (string_of_intterm it)
  | Minus (ie, it) -> (string_of_intexpr ie) ^ "-" ^ (string_of_intterm it)
  | Term (it) -> (string_of_intterm it)

and string_of_intterm intterm = match intterm with
  | Times (it, ic) -> (string_of_intterm it) ^ "*" ^ (string_of_intfactor ic)
  | Devide (it, ic) -> (string_of_intterm it) ^ "/" ^ (string_of_intfactor ic)
  | Factor (ic) -> (string_of_intfactor ic)

and string_of_intfactor intfactor = match intfactor with
  | Expr (ie) -> "(" ^ (string_of_intexpr ie) ^ ")"
  | Int (i) -> (string_of_int i)
  | Variable (v) -> "@" ^ v

let string_of_intvariable intvariable = match intvariable with
  | IntB (i) -> (string_of_int i)
  | VariableB (v) -> "@" ^ v

let rec string_of_boolexpr boolexpr = match boolexpr with
  | And (be1, be2) -> (string_of_boolexpr be1) ^ " & " ^ (string_of_boolexpr be2)
  | Or (be1, be2) -> (string_of_boolexpr be1) ^ " | " ^ (string_of_boolexpr be2)
  | Factor (bf) -> (string_of_boolfactor bf)
  | NegFactor (bf) -> "!" ^ (string_of_boolfactor bf)

and string_of_boolfactor boolfactor = match boolfactor with
  | Bool (b) -> (string_of_bool b)
  | IntEq (iv1, iv2) -> (string_of_intvariable iv1) ^ " == " ^ (string_of_intvariable iv2)

let string_of_literal literal = match literal with
  | String (s) -> s
  | Int (i) -> (string_of_int i)
  | IntExpr (ie) -> (string_of_intexpr ie)
  | BoolExpr (be) -> (string_of_boolexpr be)

let rec string_of_element element = match element with
  | Literal (l) -> (string_of_literal l)
  | Variable (v) -> "@" ^ v
  | Context (c) -> "$" ^ c
  | Wrapping (w) -> "<" ^ (string_of_element w) ^ ">"
  | Multiset (m) -> "{" ^ (string_of_elementlist_multiset m) ^ "}"
  | Sequence (s) -> "[" ^ (string_of_elementlist_sequence s) ^ "]"

and string_of_elementlist_multiset list = match list with
  | [] -> ""
  | first :: [] -> (string_of_element first)
  | first :: rest -> (string_of_element first) ^ ", " ^ (string_of_elementlist_multiset rest)

and string_of_elementlist_sequence list = match list with
  | [] -> ""
  | first :: [] -> (string_of_element first)
  | first :: rest -> (string_of_element first) ^ ", " ^ (string_of_elementlist_sequence rest)

let string_of_data data = match data with
  | Multiset (m) -> (string_of_elementlist_multiset m)
  | _ -> raise (Sys_error "string_of_data")

(* --- main --- *)
let text_of_instruction instruction = match instruction with
  | Inst (d1, be, d2) -> "Inst (" ^ (text_of_element d1) ^ ", " ^ (text_of_literal be) ^ ", " ^ (text_of_element d2) ^ ")"

let apply_instruction data instruction = match instruction with
  | Inst (head, guard, body) -> Eval.applyInstruction data head guard body

let data = ref (Multiset ([]))

let _ =
  print_string "# ";flush stdout;
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let (r, d) = apply_instruction !data (Parser.instruction_eol Lexer.token lexbuf) in
      (
        if r then (
          data := d;
          print_string (string_of_data d)
        ) else (
          print_string "Instruction failed."
        );
        print_newline();
        print_string "# ";
        flush stdout
      )
    done
  with Lexer.Eof ->
    exit 0