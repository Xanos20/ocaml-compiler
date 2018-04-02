(*Expressions for lexer*)
type exp =
| EInt of int
| EAdd   of  exp * exp  (* add takes one arg which is a pair of expressions *)
| ESubt  of  exp * exp
| EMult  of  exp * exp
| EDivi  of  exp * exp
| ETrue  
| EFalse 
| ELessThanOrEquals of exp * exp
| EIf    of  exp * exp * exp

  
(* colon= returns int *)
let rec interpret (e:exp) : int =
  match e with
  | EInt  n                    -> n
  | EAdd              (e1, e2) -> interpret e1 + interpret e2
  | ESubt             (e1, e2) -> interpret e1 - interpret e2
  | EMult             (e1, e2) -> interpret e1 * interpret e2
  | EDivi             (e1, e2) -> interpret e1 / interpret e2
  | ETrue                      -> 0
  | EFalse                     -> 1
  (* turn bool to int, some assistance from Johnathon*)
  | ELessThanOrEquals (e1, e2) -> if interpret e1 <= interpret e2 then 0 else 1
  | EIf           (e1, e2, e3) -> if interpret e1 = 0 then interpret e2 else interpret e3    
   
