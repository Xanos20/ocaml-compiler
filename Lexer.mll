{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [
    ( "(", LPAREN     )
  ; ( ")", RPAREN     )
  ; ( "+", PLUS       )
  ; ( "-", SUBT       )
  ; ( "*", MULT       )
  ; ( "/", DIVD       )
  ; ( "if", IF        )
  ; ( "<=", LESSEQLS  )
  ; ( "==", ISEQUAL   )
  ; ( "=" , EQUALS    )
  ; ( "->", ARROW     )
  ]


let keywords : (string * Parser.token) list =
  [
    ( "in", IN        )
 (* ; ( "var ", VAR )  *)
  ; ( "let" , LET     )
  ; ( "fun" , FUN     )
  ; ( "fix" , FIX     )
  ; ( "funcall", FUNCALL )
  ; ( "and" , AND     )
  ; ( "or"  , OR      )
  ; ( "xor" , XOR     )
  ; ( "not" , NOT     )
  ; ( "nand", NAND    )
  ; ( "nor" , NOR     )
  ; ( "nxor", NXOR    )
  ]

let create_keyword lexbuf =
  let str = lexeme lexbuf in
  List.assoc str keywords

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols



let create_bool lexbuf = lexeme lexbuf |> bool_of_string
let create_int lexbuf = lexeme lexbuf |> int_of_string

}

let a_keyword = "in" | "let" | "fun" | "fix" | "funcall" | "and" | "or" | "xor" | "not" | "nand" | "nor" | "nxor"
let a_symbol = '(' | ')' | '+' | '-' | '*' | '/' | "<=" | "==" | '=' | "->"


let newline       = '\n' | ('\r' '\n') | '\r'
let whitespace    = ['\t' ' ']
let digit         = ['0'-'9']
let variable_name = ['a'-'z'  'A'-'Z'] 


rule token = parse
  | eof                                                          { EOF }
  | digit+                                                       { INT (int_of_string (lexeme lexbuf)) }
  | whitespace+ | newline+                                       { token lexbuf }
  | "true" | "false"                                             { BOOL (bool_of_string (lexeme lexbuf)) }
  | a_symbol 							 { create_symbol lexbuf }  
  | a_keyword 							 { create_keyword lexbuf }
  | variable_name                                                { VAR (lexeme lexbuf) }
  | _ as c                                                       { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }