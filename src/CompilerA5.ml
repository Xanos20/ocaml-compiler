open Pervasives
open Lexer
open Parser
open Lang

let main () = 
   Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel 
    |> Parser.prog Lexer.token 
    |> Lang.interpret  
    |> Lang.string_of_exp 
    |> print_endline 
    
  
let _ = if !Sys.interactive then () else main ()
   
