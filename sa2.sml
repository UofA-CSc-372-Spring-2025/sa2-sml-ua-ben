(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Benjmain Seckeler                                    *)
(* Time spent on HW6: 4
*)

(* Collaborators and references:
* smlhelp.github.io/
* chatgpt
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () = 
    Unit.checkExpectWith Bool.toString "mynull [1,2] should be false"
	(fn () => mynull [1, 2])
    false

val () = 
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
	(fn () => mynull [1])
    false

(**** Problem B ****)

val vowels = [#"a", #"e", #"i", #"o", #"u"]

fun isVowel (c, []) = false
  | isVowel (c, x::xs) = (c = x) orelse (isVowel (c, xs));

fun firstVowel [] = false
  | firstVowel (x::_) = isVowel(x, vowels);

val () =
    Unit.checkExpectWith Bool.toString "firstvowel '' should be false"
    (fn () => firstVowel [])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstvowel 'ick' should be true"
    (fn () => firstVowel [#"i",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstvowel 'yck' should be false"
    (fn () => firstVowel [#"y",#"c",#"k"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstvowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

(**** Problem C ****)

fun reverse [] = []
  | reverse (x) = foldl(fn (x, acc) => x :: acc) [] x;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [])
  []

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1, 32, 4, 5] should be [5, 4, 32,1]"
  (fn () => reverse [1, 32, 4, 5])
  [5,4,32,1]

(**** Problem D ****)

fun minlist [] = raise Match
  | minlist (x) = foldl(fn(x, acc) => Int.min(x, acc)) 0 x; 

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,-33,4,0] should be -33"
  (fn () => minlist [1,2,~33,4,4])
  ~33

(**** Problem E ****)

exception Mismatch

fun zip [] [] = []
  | zip (x::xs) (y::ys) = (x, y) :: zip xs ys
  | zip (x::xs) [] = raise Mismatch
  | zip [] (x::xs) = raise Mismatch
  ;

fun tupleToString(x, y) = "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")";

val a = zip [3,2] [2,1];
printTuple(List.nth (a, 1));

(* TODO : Test cases for this *)

(*
val () =
  Unit.checkExnWith (Unit.listString tupleToString)
  "missmatched zip [] [] should raise an exception"
  (fn () => zip [3,2,1] [1, 2])
  [(1,2)]
  *)

(*
val () =
  Unit.checkExpectWith
  "Small testcase #0"
  (fn () => zip [0, 1] [1, 0])
  [(0, 1), (1, 0)]
  *)
(**** Problem F ****)
(*

  | zip [] x = raise Mismatch
  | zip x [] = raise Mismatch
  | zip _ x = raise Mismatch
  | zip x _ = raise Mismatch


fun concat xs = xs
*)
(**** Problem G ****)
(*
fun isDigit _    = false;
*)
(**** Problem H ****)
(*
fun isAlpha c = false
*)
(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)
(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
