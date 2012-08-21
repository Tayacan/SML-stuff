(*Problem 1:
* Add all the natural numbers below one 
* thousand that are multiples of 3 or 5
* *)

load "Int";

(*First of all, we need to be able to sum:*)
fun sum xs = foldl op+ 0 xs;

(*Second, we need to figure out if a number is
* a multiple of 3 or 5*)
fun filterp x = (x mod 5 = 0) orelse (x mod 3 = 0);

(*Third, we need a list to filter*)
val range = List.tabulate(1000, fn i => i);

(*And last, we need to tie it all together!*)
val euler1 = sum (List.filter filterp range);

(*Problem 2:
* The sum of all even fibonacci-numbers
* below 4 millions
* *)

(*First, we make a list of all the fibonacci
* numbers below 4 millions*)
fun fib x1 x2 = if (x2 + x1 + x2) < 4000000 
                then (x1 + x2) :: fib x2 (x1 + x2)
                else [(x1 + x2)];

(*We need to know if a number is even*)
fun evenp x = x mod 2 = 0;

(*Then, filter and sum!*)
val euler2 = sum (List.filter evenp (fib 0 1));

(*Problem 3:
* The largest prime factor of 600851475143
* *)

(*We need to be able to find all the factors of a number*)
fun factors x = List.filter (fn i => x mod i = 0) (List.tabulate (x div 2, fn i => i+1));

(*Then, we find the ones that are primes, and take the largest one.*)
fun max xs = foldl Int.max 0 xs;

(* Helper to see if a number is a prime *)
fun isPrime 1 = true
  | isPrime x = length (factors x) = 1;

(* MosML can't do large ints, so here's a general function for
finding the largest prime factor of a number...*)
fun euler3 x = max (List.filter isPrime (factors x));
