
(* The type of tokens. *)

type token = 
  | XOR
  | VAR of (string)
  | SUBT
  | RPAREN
  | PLUS
  | OR
  | NXOR
  | NOT
  | NOR
  | NAND
  | MULT
  | LPAREN
  | LET
  | LESSEQLS
  | ISEQUAL
  | INT of (int)
  | IN
  | IF
  | FUNCALL
  | FUN
  | FIX
  | EQUALS
  | EOF
  | DIVD
  | BOOL of (bool)
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
