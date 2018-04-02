# pegs
## Dependencies
Have `opam` installed.
## Instructions for use
The easiest way to build this library using Ocamlbuild. If you don't have this, just use `opam install Ocamlbuild` on the command line.
Once you've done that, it is as simple as:
`ocamlbuild run.byte
./run.byte`.
My intention with this library is that anyone can modify `triangle.ml` using `triangle.mli` as a template, perhaps making the game more graphically sophisticated than simple use of the command line.
## triangle.mli
This mli file contains the abstract structure of the game. Using this, it can be implemented a number of different ways. All methods and functions are explained in the comments of this file.
## triangle.ml
This module is my implementation of the game, which prints the board on `stdout`.
## pegs.ml
This module takes controls in from the command line, implementing the game fully.
## run.ml
This just runs my implementation of the game.