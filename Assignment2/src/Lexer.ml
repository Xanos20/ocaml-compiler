type token =
  | TInt of int
  | TLParen
  | TRParen
  | TPlus
  | TSubt
  | TMult
  | TDivi
  | TTrue
  | TFalse
  | TLessThanOrEquals
  | TIf

let string_of_token (t:token) : string =
  match t with
  | TInt n            -> string_of_int n
  | TLParen           -> "("
  | TRParen           -> ")"
  | TPlus             -> "+"
  | TSubt             -> "-"
  | TMult             -> "*"
  | TDivi             -> "/"
  | TTrue            -> "true"
  | TFalse           -> "false"
  | TLessThanOrEquals -> "<="
  | TIf               -> "if"

(* parameter = name:type  and list is a generic type*)
let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)
  (*returns a string with commas after each char*)

(* Peeks at the head of the stream without advancing it forward *)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pops the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next

(* Returns true iff this stream still has elements left *)
let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false

let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'


let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

  
let is_alpha (ch:char) : bool =
  let code = Char.code ch in
  (code >= 97 && code <= 122) || (code <= 65 && code <= 90)


(* Note: lex contains two nested helper functions, lex_num and go *)
let lex (src:char Stream.t) : token list =
  let rec lex_num acc =
    if is_digit (peek src) then
    (*string concat*)
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      int_of_string acc
  in
  let rec lex_string acc =
    if is_alpha (peek src) then
    (*string concat*)
      lex_string (acc ^ (Char.escaped (advance src)))
    else
        acc
  in
let rec go () =
    begin if not (is_empty src) then
      let ch = peek src in
      (* Note: the |> operator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '(' -> advance src |> ignore; TLParen :: go ()
      | ')' -> advance src |> ignore; TRParen :: go ()
      | '+' -> advance src |> ignore; TPlus   :: go ()
      | '-' -> advance src |> ignore; TSubt   :: go ()
      | '*' -> advance src |> ignore; TMult   :: go ()
      | '/' -> advance src |> ignore; TDivi   :: go ()
      | 't' -> advance src |> ignore; if (advance src = 'r') then
                             if (advance src = 'u') then
			       if (advance src = 'e') then TTrue :: go ()
			         else failwith "expected e"
			     else failwith "expected u"
			   else failwith "expected r"
      | 'f' -> advance src |> ignore; (if advance src = 'a' then
                             (if advance src = 'l' then 
                               (if advance src = 's' then 
                                 (if advance src = 'e' then TFalse :: go ()
                                  else failwith "expected 'e'")
                                else failwith "expected 's'")
                              else failwith "expected 'l'")
                            else failwith "expected 'a'")
      | '<' -> advance src |> ignore; if (advance src = '=') then TLessThanOrEquals :: go () else failwith "expected = for LessThanOrEquals"
      | 'i' -> advance src |> ignore; if (advance src = 'f') then TIf               :: go() else failwith "expected f for if"
      | _   ->
        begin if is_whitespace ch then
          (*skip over whitespace*)
          begin advance src |> ignore; go () end
        else if is_digit ch then
          let n = lex_num "" in
          TInt n :: go ()
        else
          failwith (Printf.sprintf "Unexpected character found: %c" ch)
        end
    else
      []
    end
  in
    go ()
