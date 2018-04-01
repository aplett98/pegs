open Sys

type location

type pin = location * bool

exception InvalidLocation
exception InvalidJump
exception InvalidUndo
exception InvalidAction
exception EndGame
exception WinGame


(* valid loc -- Check whether a given location is valid. *)
val valid : location -> location

(* game s -- Return an object representing a game board, with 
  starting location s *)
class game : location -> object
  
  (* Return only a valid location, otherwise raise InvalidLocation *)
  method valid : location -> location

  (* Render the original board *)
  method base : unit

  (* Raise WinGame if the game is won *)
  method win : unit

  (* Bring the board to a starting state *)
  method reset : location -> unit

  (* Render the current state of the board *)
  method board : unit

  (* Determine whether or not a given location is empty *)
  method is_empty : location -> bool

  (* Empty a given location *)
  method empty : location -> unit

  (* Put a pin in an empty location *)
  method insert : location -> unit

  (* Render the board in a starting state *)
  method start : location -> unit

  (* Jump one pin over another, if valid, and render the board *)
  method jump : location -> location -> unit

  (* Undo the last move *)
  method undo : unit

end



