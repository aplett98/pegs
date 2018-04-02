open Sys

type location = 
  | A of int
  | B of int
  | C of int
  | D of int
  | E of int

type pin = location * bool

type triangle = pin list

exception InvalidLocation
exception InvalidJump
exception InvalidUndo
exception InvalidAction
exception EndGame
exception WinGame

let valid (loc : location) : location =
  let ls x y = x >= 0 && x <= y in
  match loc with
  | A n -> if ls n 0 then loc else raise InvalidLocation
  | B n -> if ls n 1 then loc else raise InvalidLocation
  | C n -> if ls n 2 then loc else raise InvalidLocation
  | D n -> if ls n 3 then loc else raise InvalidLocation
  | E n -> if ls n 4 then loc else raise InvalidLocation


(* game object: keeps track of one game *)

class game (s : location) =
object(this)
  val base = 
	[A 0, true; 
	 B 0, true; B 1, true; 
	 C 0, true; C 1, true; C 2, true; 
	 D 0, true; D 1, true; D 2, true; D 3, true;
	 E 0, true; E 1, true; E 2, true; E 3, true; E 4, true]
  val mutable board = []

  method valid (l : location) =
    if List.mem (l, true) base || List.mem (l, false) base 
      then l else raise InvalidLocation

  val mutable start = s

  method base = 
    let pr = board in 
    board <- base; this#board; board <- pr

  method win = 
	if List.length (List.filter (fun a -> not (snd a)) board) = 14 
	  then raise WinGame


  (* index: translates location into a number on which to perform arithmetic *)
  method index (l : location) =
    match l with
    | A n -> 0 + n
    | B n -> 1 + n
    | C n -> 3 + n
    | D n -> 6 + n
    | E n -> 10 + n

  (* reset: brings the board back to its orignal state *)
  method reset (l: location) = 
    start <- this#valid l;
	board <- base;
	this#start


  (* board: method to print the board *)
  method board = 
  	let buffer (p : pin) : string =
  	  match fst p with
  	  | A 0 -> "A:      "
  	  | B 0 -> "B:     "
  	  | C 0 -> "C:    "
  	  | D 0 -> "D:   "
  	  | E 0 -> "E:  "
  	  | _ -> "" 
  	in
  	let endline (p : pin) : string =
  	  match fst p with 
  	  | A 0 | B 1 | C 2 | D 3 -> "\n"
  	  | E 4 -> "\n   0 1 2 3 4\n"
  	  | _ -> ""
  	in
    let printpin (p : pin) = 
      let x = if snd p then "X " else "O " in
      Printf.printf "%s%s%s" (buffer p) x (endline p)
	in
	List.iter printpin board; this#win

  method is_empty (l : location) =
  	let bl = ref false in
    let index = this#index (this#valid l) in
    List.iteri (fun i p -> if i = index then bl := not (snd p)) board;
    this#win;
    !bl

  method empty (l : location) = 
    let index = this#index (this#valid l) in
    let newboard =
      List.mapi (fun i p -> if i = index then (fst p, false) else p) board
    in
    board <- newboard

  method insert (l : location) =
    let index = this#index (this#valid l) in
    let newboard =
      List.mapi (fun i p -> if i = index then (fst p, true) else p) board
    in
    board <- newboard

  method start =
    board <- base;
    this#empty start;
    this#board

  val mutable prev = [None]

  method jump (a : location) (b : location) =
    if this#is_empty (this#valid a) || this#is_empty (this#valid b)
      then raise InvalidJump else
    let vp x y = if abs (x - y) > 1 then raise InvalidJump else 
      if x = y then x else y + 1 in
    let vm x y = if abs (x - y) > 1 then raise InvalidJump else 
      if x = y then x else y - 1 in
    let va x y = if x < y then x + 2 else x - 2 in
    let v l = try this#valid l with InvalidLocation -> raise InvalidJump in
    let c : location = 
      match a, b with
      | A x, B y -> v (C (vp x y))
      | B x, C y -> v (D (vp x y))
      | C x, B y -> v (A (vm x y))
      | C x, C y -> v (C (va x y))
      | C x, D y -> v (E (vp x y))
      | D x, C y -> v (B (vm x y))
      | D x, D y -> v (D (va x y))
      | E x, D y -> v (C (vm x y))
      | E x, E y -> v (E (va x y))
      | _, _ -> raise InvalidJump
    in
    if not (this#is_empty c) then raise InvalidJump;
    prev <- Some board :: prev;
    List.iter this#empty [a;b]; this#insert c; this#board

  method undo =
    match prev with
    | [] -> raise InvalidUndo
    | None :: _ -> raise InvalidUndo
    | Some p :: t -> board <- p; prev <- t;
    this#board
   
end

let translate (s : string) : location =
  match s with
  | "a0" -> A 0
  | "b0" -> B 0
  | "b1" -> B 1
  | "c0" -> C 0
  | "c1" -> C 1
  | "c2" -> C 2
  | "d0" -> D 0
  | "d1" -> D 1
  | "d2" -> D 2
  | "d3" -> D 3
  | "e0" -> E 0
  | "e1" -> E 1
  | "e2" -> E 2
  | "e3" -> E 3
  | "e4" -> E 4
  | _ -> raise InvalidLocation

