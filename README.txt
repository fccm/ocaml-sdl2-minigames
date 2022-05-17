Here are some minimalist games made for beginners who want to learn OCaml.

In each directory several files represent an evolution of the game
from a very minimal state (file number 0) and the following files add new
elements, features or refinements of the source code.

You can compare two following files with the command:
diff -u shmup7.ml shmup8.ml

The documentation is provided into separated files with the same number
and -en for English language and -fr for French language.

All the examples need OCamlSDL2 to work,  
to run one of these mini-games you can use this command:

ocaml -I ../OCamlSDL2/src sdl2.cma minigame.ml
