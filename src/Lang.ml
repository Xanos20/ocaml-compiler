(**)


(*Expressions for lexer*)
type exp =
| EInt      of int
| EBool     of bool
| EFun      of string * exp
| EVar      of string
| EAdd      of  exp * exp  
| ESubt     of  exp * exp
| EMult     of  exp * exp
| EDivd     of  exp * exp
| ELessEqls of exp * exp
| EIsEqual  of exp * exp
| EIf       of exp * exp * exp
| ELet      of string * exp * exp
| EFunCall  of exp * exp
(*| ERecFun   of    *)   (* TODO: Add ERecFun*)
| EAnd      of exp * exp
| ENand     of exp * exp
| EOr       of exp * exp
| ENor      of exp * exp
| EXor      of exp * exp
| ENXor     of exp * exp
| ENot      of exp



(* AST Constructor *)
let rec exp_to_str (e:exp) : string =
  match e with
  | EInt  n                -> string_of_int n
  | EVar  v                -> v
  | EBool b                -> string_of_bool b                     
  | EAdd      (e1, e2)     -> "( + "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ESubt     (e1, e2)     -> "( - "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EMult     (e1, e2)     -> "( * "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EDivd     (e1, e2)     -> "( / "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"                         
  | ELessEqls (e1, e2)     -> "( <= "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EIsEqual  (e1, e2)     -> "( == "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EIf       (e1, e2, e3) -> "( if "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " "    ^ exp_to_str e3 ^ " )"
  | ELet      (s, e2, e3)  -> "( let "  ^ s ^ " = "        ^    exp_to_str e2  ^ " in " ^ exp_to_str e3 ^ " )"
  | EFun      (s, e1)      -> "( fun "  ^ s ^ " -> "       ^    exp_to_str e1  ^ " )" 
  | EFunCall  (e1, e2)     -> "( "      ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EAnd      (e1, e2)     -> "( && "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENand     (e1, e2)     -> "( NAND " ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EOr       (e1, e2)     -> "( || "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENor      (e1, e2)     -> "( NOR "  ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EXor      (e1, e2)     -> "( XOR "  ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENXor     (e1, e2)     -> "( NXOR " ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENot      (e1)         -> "( ! "    ^ exp_to_str e1 ^ " )"

(* Turns an expression into a string to be appended to the AST *)
let rec exp_to_string (e:exp) : string =
  exp_to_str(e) ^ String.make 1 '\n'
  

             
  
(* subst :: value -> string -> exp -> exp
   Parameters:
     The value to substitute.
     The variable to substitute for (as a string or equivalent)
     The expression to substitute into
   Output:
     Produces an expression that is the result of substituting the value for the variable everywhere that
     variable occurs inside the given expression. 
 *)        
let rec subst (s:string) (e1:exp) (e2:exp) : exp = 
  match e2 with
    | EInt   n            -> e2
    | EBool  b            -> e2
    (*| EFun  (param, body) -> if ((compare param s) = 0) then e2 else EFun (param, subst (s, e1, body)) 
    | EFunCall (e11, e12) ->  match (interpret e11) with
                             | EFun (par, bod) -> EFun (subst (s, param, e1), subst(s, e1, bod))  (* TODO Fix *)
                             | _  -> e *)
    | EIf   (e4, e5, e6)  -> EIf (subst(s)(e1)(e4), subst(s)(e1)(e5), subst(s)(e1)(e6)) 
    | EVar  (var)         -> if ((compare var s) = 0) then e1 else e2
    | ELet  (var, e4, e5) -> ELet  (var, subst(s)(e1)(e4), subst(s)(e1)(e5))
    | EAdd  (i1, i2)      -> EAdd  (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | ESubt (i1, i2)      -> ESubt  (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | EMult (i1, i2)      -> EMult  (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | EDivd (i1, i2)      -> EDivd  (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | ELessEqls (i1, i2)  -> ELessEqls (subst(s)(e1)(i1), subst(s)(e1)(i2))
    | EIsEqual  (i1, i2)  -> EIsEqual  (subst(s)(e1)(i1), subst(s)(e1)(i2))
    | EAnd      (b1, b2)  -> EAnd  (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENand     (b1, b2)  -> ENand  (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | EOr       (b1, b2)  -> EOr  (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENor      (b1, b2)  -> ENor  (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | EXor      (b1, b2)  -> EXor (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENXor     (b1, b2)  -> ENXor  (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENot      (b1)      -> ENot (subst(s)(e1)(b1))


let is_int (e:exp) : bool =
  match e with
  | EInt int -> true
  | _        -> false



type val:
      | VInt of int
      | VBool of bool
      | VFun of string * exp
  

              
(* The interpreter that evaluates the output from the parsing process *)
let rec interpret (e:exp) : exp =
  print_endline (exp_to_str e);
  match e with
  | EInt  n        -> e
  | EBool b        -> e
  | EFun (s, e1)   -> e
  (* Arithmatic Operators *) (*
  | EAdd  (e1, e2) -> EInt((interpret e1) + (interpret e2))                
  | ESubt (e1, e2) -> EInt((interpret e1) - (interpret e2))
  | EMult (e1, e2) -> EInt((interpret e1) * (interpret e2))
  | EDivd (e1, e2) -> EInt((interpret e1) / (interpret e2))  *)
  (* Comparisons, some assistance from Johnathon*)
  | ELessEqls (e1, e2)   -> if (interpret e1 <= interpret e2) then EBool(true) else EBool(false)
  | EIsEqual  (e1, e2)   -> if (interpret e1 = interpret e2)  then EBool(true) else EBool(false)
  | EIf   (e1, e2, e3)   -> if (interpret e1 = EBool(true) )  then interpret e2 else interpret e3
  | ELet   (s, e2, e3)   -> interpret (subst(s)(e2)(e3))  (* TODO: fix subst *)
  | EFunCall (e1, e2) -> match (interpret e1) with
                         | EFun (par, body) -> let v2 = interpret e2 in
                                               e (* TODO *)
                         | _                -> failwith "error"
                         (*| ERecFun (fun, arg) ->       *)                 
  (*Logic Operators *)
  | EAnd  (e1, e2) -> if (interpret e1 = EBool(true)  && interpret e2 = EBool(true) ) then EBool(true) else EBool(false)
  | ENand (e1, e2) -> if (interpret e1 = EBool(true)  && interpret e2 = EBool(true) ) then EBool(true) else EBool(false)
  | EOr   (e1, e2) -> if (interpret e1 = EBool(true)  || interpret e2 = EBool(true) ) then EBool(true) else EBool(false)
  | ENor  (e1, e2) -> if (interpret e1 = EBool(false) && interpret e2 = EBool(false)) then EBool(true) else EBool(false)
  | EXor  (e1, e2) -> if (interpret e1 = EBool(true)  && interpret e2 = EBool(false) ) || (interpret e1 = EBool(false) && interpret e2 = EBool(true)) then EBool(true) else EBool(false)
  | ENXor (e1, e2) -> if (interpret e1 = EBool(false) && interpret e2 = EBool(false)) || (interpret e1 = EBool(true) && interpret e2 = EBool(true)) then EBool(true) else EBool(false)
  | ENot  (e1)     -> if (interpret e1 = EBool(false)) then EBool(true) else EBool(false)
                 

   
   
