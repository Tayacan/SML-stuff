(* Excercises from chapter 2*)

load "Math";
load "Int";

(*Exc 1: Pascal's triangle*)
fun bin (0,_) = 0
  | bin (n,0) = bin (n,n)
  | bin (n,k) = if n = k then 1
		else bin (n - 1, k - 1) + bin (n - 1, k);

(*Exc 2: pow*)
fun pow (s,0) = ""
  | pow (s,n) = s ^ pow (s,(Int.abs n) - 1);

(*Exc 3: isIthChar*)
fun isIthChar ("",_,_) = false
  | isIthChar (str,i,ch) = String.sub (str,i) = ch;
