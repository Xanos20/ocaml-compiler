let main () =
  (* if Sys.argv.(2) = "-parse" then  *)
   (*Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.interpret   
(*  |> print_string *)
    (* else *) *)
   Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.interpret  
    |> Lang.exp_to_string
         (*  |> print_endline *)
    
  (*
let _ = if !Sys.interactive then () else main ()
   *)
