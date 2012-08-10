(* Parses a list of numbers and ranges 
into a sorted list of numbers
Input example: "1-5,7,12-15,8"
Output example: [1,2,3,4,5,7,8,12,13,14,15]
*)

load "Int";

(*Take a list and split it into sublists, 
using sep as a delimeter*)
fun split [] _ = [[]]
  | split (x::xs) sep = 
let
	val rest = split xs sep
in
	if x = sep
	then [] :: rest
	else (x :: hd rest) :: tl rest
end;

(*Use split to turn a string into substrings,
seperated by commas*)
fun makeStringList s = map implode (split (explode s) #",");

(*Evaluate a range from a char-list*)
fun makeRange [] = []
  | makeRange (c::cs) = 
let
	val numbers = map (Option.valOf o Int.fromString o implode) (split (c::cs) #"-")
	fun range a b = List.tabulate (b + 1 - a, fn i => i + a)
in
	range (List.nth (numbers, 0)) (List.nth (numbers, 1))
end;

(*Figure out if a char is a digit*)
val digitp = 
let
	fun inRange (min,max) v = ((v >= min) andalso (v <= max))
in
	inRange (Char.ord #"0", Char.ord #"9") o Char.ord
end;

(*Sort a list*)
fun sort [] = []
  | sort (x::xs) =
let
	val smallerSorted = List.filter (fn n => n <= x) (sort xs)
	val biggerSorted = List.filter (fn n => n > x) (sort xs)
in
	smallerSorted @ [x] @ biggerSorted
end;

(*Tie it all together*)
fun parseString "" = []
  | parseString s = 
let
	(* Delete item from list*)
	fun delete _ [] = []
	  | delete item (x::xs) = if item = x
				  then delete item xs
				  else x :: (delete item xs)
	(* Remove duplicates from list*)
	fun remDups [] = []
	  | remDups (x::xs) = x :: (remDups (delete x xs))
	(*Return a range or number, depending on input string*)
	fun parseStrings [] = []
	  | parseStrings (x::xs) = if List.all digitp (explode x)
				   then Option.valOf (Int.fromString x) :: parseStrings xs
				   else (makeRange (explode x)) @ (parseStrings xs)
in
	remDups (sort (parseStrings (makeStringList s)))
end;
