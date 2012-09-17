(*load "Random";
load "Int";
load "TextIO";*)

val gen = Random.newgen ();
val start = Random.range(1,100) gen;

fun game (s,n) =
let
  val _ = print s
  val ins = Option.valOf (Int.fromString (TextIO.inputLine TextIO.stdIn))
in
  if ins = n
  then game ("Correct! Guess the new number\n",Random.range(1,100) gen)
  else if ins < n 
       then game ("higher\n",n)
       else game ("lower\n",n)
end;

val _ = game ("Try to guess the number\n",start)
