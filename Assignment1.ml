let blankFlag = ref false
let listArgs = ref []

let args =
[ ("-length", Arg.Set blankFlag, "help string")
]


let checkCond x =
if !blankFlag then string_of_int (String.length x) else x


let rec print l =
  match l with
  | []      -> ()
  | x :: l' -> print_endline(checkCond x); print l'


let main() =
  Arg.parse args (fun s -> listArgs:= !listArgs@[s]) "usage string";
  print !listArgs



let _ = main ()
