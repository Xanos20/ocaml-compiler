(*Expressions for lexer*)
type exp =
| EInt of int
| EAdd   of  exp * exp  
| ESubt  of  exp * exp
| EMult  of  exp * exp
| EDivd  of  exp * exp
| ETrue  
| EFalse 
| ELessEqls of exp * exp
| EIf       of exp * exp * exp


let rec exp_to_str (e:exp) : string =
  match e with
  | EInt  n                -> string_of_int n
  | ETrue                  -> string_of_bool true
  | EFalse                 -> string_of_bool false
  | EAdd      (e1, e2)     -> "(+ "^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^")"
  | ESubt     (e1, e2)     -> "(- "^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^")"
  | EMult     (e1, e2)     -> "(* "^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^")"
  | EDivd     (e1, e2)     -> "(/ "^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^")"
  | ELessEqls (e1, e2)     -> "(<= "^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^")"
  | EIf       (e1, e2, e3) -> "(if "^ exp_to_str e1 ^ " " ^ exp_to_str e2 ^ " " ^ exp_to_str e3 ^ ")"


let rec string_of_exp (e:exp) : string =
  exp_to_str(e) ^ String.make 1 '\n'
  
     
let rec interpret (e:exp) : int =
  match e with
  | EInt  n                    -> n
  | EAdd              (e1, e2) -> interpret e1 + interpret e2
  | ESubt             (e1, e2) -> interpret e1 - interpret e2
  | EMult             (e1, e2) -> interpret e1 * interpret e2
  | EDivd             (e1, e2) -> interpret e1 / interpret e2
  | ETrue                      -> 0
  | EFalse                     -> 1
  (* turn bool to int, some assistance from Johnathon*)
  | ELessEqls (e1, e2) -> if interpret e1 <= interpret e2 then 0 else 1
  | EIf   (e1, e2, e3) -> if interpret e1 = 0 then interpret e2 else interpret e3    
   
