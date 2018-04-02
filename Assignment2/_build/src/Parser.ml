open Lang
open Lexer

   
let rec peek : token list -> token = List.hd
let rec advance : token list -> token list = List.tl

(*Looks at head of token list for right token*)
let rec consume (t:token) (toks:token list) : token list =
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  
  else
    match peek toks with
    | TTrue  -> (ETrue , advance toks)
	                                                                   
    | TFalse -> (ETrue , advance toks)
                                                           
    | TInt n -> (EInt n, advance toks)
               
    (* if you have seen a left paren*)
    | TLParen -> begin
        let toks       = consume TLParen toks in
        (match peek toks with
        | TPlus -> let toks = consume TPlus toks in
                   let (e1, toks) = parse toks in
                   let (e2, toks) = parse toks in
                   let toks       = consume TRParen toks in (EAdd (e1, e2), toks)
         
        | TSubt -> let toks = consume TSubt toks in
	           let (e1, toks) = parse toks in
	           let (e2, toks) = parse toks in
	           let toks       = consume TRParen toks in (ESubt (e1, e2), toks)
                                                          
	| TMult -> let toks = consume TMult toks in
	           let (e1, toks) = parse toks in
	           let (e2, toks) = parse toks in
	           let toks       = consume TRParen toks in (EMult (e1, e2), toks)
                                                          
        | TDivi -> let toks = consume TDivi toks in
	           let (e1, toks) = parse toks in
	           let (e2, toks) = parse toks in
	           let toks       = consume TRParen toks in (EDivi (e1, e2), toks)                                                       
                                                           
	| TLessThanOrEquals -> let toks = consume TLessThanOrEquals toks in
	                       let (e1, toks) = parse toks in
                               let (e2, toks) = parse toks in
	                       let toks       = consume TRParen toks in (ELessThanOrEquals (e1, e2), toks)
                                                                      
        | TIf -> let toks = consume TIf toks in
	         let (e1, toks) = parse toks in
                 let (e2, toks) = parse toks in
	         let (e3, toks) = parse toks in
	         let toks       = consume TRParen toks in (EIf (e1, e2 , e3), toks)
	)
      end
    | t      -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
