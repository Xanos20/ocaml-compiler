(**)
(* open Pervasives *)



type val:
      | VInt of int
      | VBool of bool
      | VFun of string * exp

type exp =
| EInt      of int
| EBool     of bool
| EFun      of string * typ * exp
| EVar      of string
| EAdd      of exp * exp  
| ESubt     of exp * exp
| EMult     of exp * exp
| EDivd     of exp * exp
| ELessEqls of exp * exp
| EIsEqual  of exp * exp
| EIf       of exp * exp * exp
| ELet      of string * exp * exp 
| EFunCall  of exp * exp
| EAnd      of exp * exp
| ENand     of exp * exp
| EOr       of exp * exp
| ENor      of exp * exp
| EXor      of exp * exp
| ENXor     of exp * exp
| ENot      of exp


let is_value (e:exp) : bool =
  match e with
  | VInt _         -> true
  | VBool _        -> true
  | VFun _         -> true
  | VFix _         -> true
  | _              -> false


             
(* AST Constructor *)
let rec exp_to_str (e:exp) : string =
  match e with
  (* | E v -> string_of_value v            TODO FILL IN WITH VALUE TYPES *)
  | 
  | EAdd      (e1, e2)     -> "( + "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ESubt     (e1, e2)     -> "( - "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EMult     (e1, e2)     -> "( * "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EDivd     (e1, e2)     -> "( / "    ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"         
  | ELessEqls (e1, e2)     -> "( <= "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EIsEqual  (e1, e2)     -> "( == "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EIf       (e1, e2, e3) -> "( if "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " "    ^ exp_to_str e3 ^ " )"
  | ELet      (s, e2, e3)  -> "( let "  ^ s ^ " = "        ^    exp_to_str e2  ^ " in " ^ exp_to_str e3 ^ " )"
  | EFunCall  (e1, e2)     -> "( "      ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EAnd      (e1, e2)     -> "( && "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENand     (e1, e2)     -> "( NAND " ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EOr       (e1, e2)     -> "( || "   ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENor      (e1, e2)     -> "( NOR "  ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | EXor      (e1, e2)     -> "( XOR "  ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENXor     (e1, e2)     -> "( NXOR " ^ exp_to_str e1 ^ " " ^ exp_to_str e2  ^ " )"
  | ENot      (e1)         -> "( ! "    ^ exp_to_str e1 ^ " )"
(* TODO FIX string_of_value (e:value) : string =
  match v with
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VFun (s, e1) -> "( fun "  ^ s ^ " -> "       ^    exp_to_str e1  ^ " )"
  | VFix (e1, e2, e3, t1, t2) -> "(fix " ^ exp_to_str e1 ^ " (" ^ exp_to_str e2 ^ ": "
    ^ (string_of_value t2) ^ ") : " ^ (string_of_value t2) ^ " -> "
    ^ exp_to_str e3 ^ " )" 
 *)
                       
(* Turns an expression into a string to be appended to the AST *)
let rec string_of_exp (e:exp) : string =
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
    | EVal v ->
       begin
         match v with
         | VInt n -> e2
         | VBool b -> e2
         | VFun (param, body)   -> if ((compare param s) = 0) then e2 else VFun (param, subst (s)(e1)(body) )
         | VFix (name, param, body) -> if ((compare param s = 0) && (compare name s = 0)) then e2 else VFun (param, subst (s)(e1)(body))                                                              
       end
    | EVar  (var)         -> if ((compare var s) = 0) then e1 else e2
    | EFunCall (f, e2)    -> EFunCall  ((subst s e1 f), (subst s e1 e2))
    | EIf   (e4, e5, e6)  -> EIf       (subst(s)(e1)(e4), subst(s)(e1)(e5), subst(s)(e1)(e6)) 
    | ELet  (var, e4, e5) -> ELet      (var, subst(var)(e1)(e4), subst(s)(e1)(e5)) 
    | EAdd  (i1, i2)      -> EAdd      (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | ESubt (i1, i2)      -> ESubt     (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | EMult (i1, i2)      -> EMult     (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | EDivd (i1, i2)      -> EDivd     (subst (s)(e1)(i1), subst(s)(e1)(i2) )
    | ELessEqls (i1, i2)  -> ELessEqls (subst(s)(e1)(i1),  subst(s)(e1)(i2) )
    | EIsEqual  (i1, i2)  -> EIsEqual  (subst(s)(e1)(i1),  subst(s)(e1)(i2) )
    | EAnd      (b1, b2)  -> EAnd      (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENand     (b1, b2)  -> ENand     (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | EOr       (b1, b2)  -> EOr       (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENor      (b1, b2)  -> ENor      (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | EXor      (b1, b2)  -> EXor      (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENXor     (b1, b2)  -> ENXor     (subst (s)(e1)(b1), subst(s)(e1)(b2) )
    | ENot      (b1)      -> ENot      (subst(s)(e1)(b1))
                           

type typ =
| TInt of int
| TBool of bool
| TFun of typ * typ
                           

let rec type_compare (t1:typ)(t2:typ) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TFun (t1, t2), TFun (t3, t4) -> (type_compare t1 t3) && (type_compare t2 t4)


let search (c:ctx)(e:exp) : typ =
  List.assoc e c
                                  

let rec typecheck (g:ctx, e:exp) : bool =
  match e with
    | TInt  t
    | TBool t
    | TFun  t
    | TFix  t
    | EAdd (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TInt) && (t2 == TInt) then
         TInt
       else
         failwith "type error for EAdd"
    | ESubt (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TInt) && (t2 == TInt) then
         TInt
       else
         failwith "type error"
    | EMult (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TInt) && (t2 == TInt) then
         TInt
       else
         failwith "type error"
    | EDivd (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TInt) && (t2 == TInt) then
         TInt
       else
         failwith "type error"
    | ELessEqls (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TInt) && (t2 == TInt) then
         TInt
       else
         failwith "type error"
    | EEqls (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TInt) && (t2 == TInt) then
         TInt
       else if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | EAnd (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | ENand (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | EOr (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | ENor (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | EXor (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | EXnor (e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let t2 = typecheck(g)(e2) in
       if (t1 == TBool) && (t2 == TBool) then
         TBool
       else
         failwith "type error"
    | ENot (e1) ->
       let t1 = typecheck(g)(e1) in
       if (t1 == TBool) then
         TBool
       else
         failwith "type error"
    | EIf (e1, e2, e3) ->
       if typecheck(g)(e1) == typecheck(g)(e2) && typecheck(g)(e1) == TBool then
         TBool
       else
         failwith "if "
    | ELet (v, e1, e2) ->
       let t1 = typecheck(g)(e1) in
       let g = (v, t1) :: g in
       typecheck(g)(e2)
    | EFun (v, t1, e1) -> 
       let g = (v, t1) :: g in
       let t2 = typecheck(g)(e1) in
       TFun(t1)(t2)         
    | EFunCall (e1, e2) ->
       let t1 = lookup g (string_of_exp e1) in
       let t2 = typecheck(g)(e2) in
       match t1 with
       | TFun(t3, t4) -> (if t3 = t2 then t4 else failwith "Type error with EFunCall")
       | _ -> failwith "Type error with EFunCall" 
    | _ -> false 

                           
              
(* The interpreter that evaluates the output from the parsing process *)
let rec interpret (e:exp) : exp =
  print_endline (string_of_exp e); 
  match e with
  | EInt  n          -> e
  | EBool b          -> e
  | EVar  v          -> failwith "Unbound Variable"
  | EFun (s, e1)     -> e
  (* Arithmatic Operators *)  
  | EAdd  (e1, e2) -> (match (interpret e1) with
                      | EInt i -> match (interpret e2) with
                                  | EInt j -> EInt (i + j)
                                  | _      -> failwith "For EAdd 2nd val not an int"
                                  | _ -> failwith "For EAdd not an int"
                      )  
  | ESubt (e1, e2) -> (match (interpret e1) with
                      | VInt i -> match (interpret e2) with
                                  | VInt j -> EInt (i - j)
                                  | _      -> failwith "For ESubt 2nd val not an int"
                                  | _ -> failwith "For ESubt not an int"
                      )
  | EMult (e1, e2) -> (match (interpret e1) with
                      | VInt i -> match (interpret e2) with
                                  | VInt j -> EInt (i * j)
                                  | _      -> failwith "For EMult 2nd val not an int"
                                  | _ -> failwith "For EMult not an int"
                      )
  | EDivd (e1, e2) -> (match (interpret e1) with
                      | VInt i -> match (interpret e2) with
                                  | VInt j -> EInt (i / j)
                                  | _      -> failwith "For EDivd 2nd val not an int"
                                  | _ -> failwith "For EDivd not an int"
                      )
  | ELessEqls (e1, e2)   -> (if (interpret e1 <= interpret e2) then VBool(true) else VBool(false) )
  | EIsEqual  (e1, e2)   -> (if (interpret e1 = interpret e2)  then VBool(true) else VBool(false) ) 
  | EIf   (e1, e2, e3)   -> (if (interpret e1 = VBool(true) )  then interpret e2 else interpret e3 )
  | ELet   (v, e1, e2)  -> ( interpret (subst (v) (e1) (e2) ) ) 
  | EFunCall (e1, e2) -> let v = interpret e1 in
                         begin
                           match v with
                           | VFun (var, e3) -> interpret (subst var (interpret e2) e3)
                           | VFix (var1, var2, e3) as vF ->
                              interpret (subst var1 vF (subst var2 (interpret e2) e3))
                           | _ -> failwith "Expecting function"
                         end
  (*Logic Operators *)
  | EAnd  (e1, e2) -> if (interpret e1 = VBool(true)  && interpret e2 = VBool(true) ) then VBool(true) else VBool(false)
  | ENand (e1, e2) -> if (interpret e1 = VBool(true)  && interpret e2 = VBool(true) ) then VBool(true) else VBool(false)
  | EOr   (e1, e2) -> if (interpret e1 = VBool(true)  || interpret e2 = VBool(true) ) then VBool(true) else VBool(false)
  | ENor  (e1, e2) -> if (interpret e1 = VBool(false) && interpret e2 = VBool(false)) then VBool(true) else VBool(false)
  | EXor  (e1, e2) -> if (interpret e1 = VBool(true)  && interpret e2 = VBool(false) ) || (interpret e1 = VBool(false) && interpret e2 = VBool(true)) then VBool(true) else VBool(false)
  | ENXor (e1, e2) -> if (interpret e1 = VBool(false) && interpret e2 = VBool(false)) || (interpret e1 = VBool(true) && interpret e2 = VBool(true)) then VBool(true) else VBool(false)
  | ENot  (e1)     -> if (interpret e1 = VBool(false)) then VBool(true) else VBool(false)
  | _  -> failwith (string_of_exp e)
                 

   
   
