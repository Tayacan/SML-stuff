(*load "Random";
load "Int";
load "TextIO";*)

val gen = Random.newgen ();

val max = ref 100;
val min = ref 1;

val n = Random.range(!min,!max) gen;


fun game s =
let
  val _ = print s
  val ins =Option.valOf (Int.fromString (TextIO.inputLine TextIO.stdIn))
in
  if ins = n
  then "Correct"
  else if ins < n 
       then game "higher\n"
       else game "lower\n"
end;

val _ = game "Try to guess the number\n"
