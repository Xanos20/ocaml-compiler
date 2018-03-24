let main () =
   Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.string_of_exp   
    |> print_string 
        (*
  Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.exp_to_str 
    |> Lang.interpret
    |> string_of_int
    |> print_endline
         *)
  

let _ = if !Sys.interactive then () else main ()
