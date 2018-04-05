open Triangle


let help = 
  "  X over Y: Jump from pin X over pin Y. (ex: A0 over B0)\n
    additionally, you can just type X Y for the same effect.\n
  undo: Undo previous move.\n
  board: Show current state of the board.\n
  reset: Restart the game.\n
  end: Quit the game.\n"

let explode (s : string) = 
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  List.map (String.make 1) (exp (String.length s - 1) [])

let parse (s : string) : string list =
  let rec word chs =
    match chs with
    | [] -> ("", [])
    | " " :: t | "\n" :: t -> ("", t)
    | h :: t -> let h1, t1 = word t in (h ^ h1, t1)
  in
  let rec bring chs =
    match chs with
    | [] -> []
    | " " :: t -> bring t
    | lst -> let h, t = word lst in h :: bring t
  in
  bring (explode s)

let rec command (lst : string list) (t : game) =
  match lst with
  | a :: "over" :: b :: [] | a :: b :: [] ->
      t#jump (translate a) (translate b)
  | ["undo"] -> t#undo
  | ["board"] -> t#board
  | ["help"] -> print_string help
  | ["reset"] -> pegs ()
  | ["end"] | ["exit"] -> raise EndGame
  | _ -> raise InvalidAction

and pegs () =
  let () = print_string ("Instructions:\n" ^ help) in
  let test = new game test_start in
  let () = test#base in
  let () = print_string "Choose starting pin: " in
  try
  	let s = String.lowercase (read_line ()) in
  	let s = translate s in
    let t = new game s in
    let () = t#start in
    let bl = ref true in
    while !bl do
      try
        let () = print_string "Make a move, or type \"Help\": " in
        let st = parse (String.lowercase (read_line ())) in
        command st t
      with
      | EndGame -> print_string "Goodbye.\n"; bl := false
      | WinGame -> print_string "Congrats! You win!\n"; bl := false
      | InvalidLocation -> print_string "Invalid location.\n"
      | InvalidJump -> print_string "Can't jump there.\n"
      | InvalidUndo -> print_string "Nothing to undo.\n"
      | InvalidAction -> 
          print_string "That action doesn't work. Try typing \"help\".\n"
    done
  with
  | InvalidLocation -> print_string "Input a valid starting pin.\n"; pegs ()
