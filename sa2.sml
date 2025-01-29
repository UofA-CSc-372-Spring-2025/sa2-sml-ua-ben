(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Benjmain Seckeler                                    *)
(* Time spent on HW6: 6
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

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString String.toString Int.toString))
  "missmatched zip [asd, tee, boo] [1, 2, 3] should raise an exception"
  (fn () => zip ["asd", "tee", "boo"] [1, 2, 3])
  [("asd",1), ("tee", 2), ("boo", 3)]

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "missmatched zip [3, 2, 1] [1, 2, 3] should raise an exception"
  (fn () => zip [3, 7, 9] [1, 2, 3])
  [(3,1), (7, 2), (9, 3)]

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "missmatched zip [3, 2, 1] [1, 2, 3] should raise an exception"
  (fn () => zip [3,2,1] [1, 2, 3])
  [(3,1), (2, 2), (1, 3)]

val () =
  Unit.checkExnWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "missmatched zip [3,2,1] [1,2] should raise an exception"
  (fn () => zip [3,2,1] [1, 2])

(*
val () =
  Unit.checkExpectWith
  "Small testcase #0"
  (fn () => zip [0, 1] [1, 0])
  [(0, 1), (1, 0)]
  *)
(**** Problem F ****)

fun concat (x::[]) = x
  | concat [] = []
  | concat (x::xs) = x @ concat xs;

	(*
	* so first we have a HEAD @ concat(TAIL)
	* bassis : empty 
	* *)

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "concat 1"
  (fn () => concat [[1], [], [2], [3, 4]])
  [1,2,3,4]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "concat 2"
  (fn () => concat [])
  []


val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "concat 3"
  (fn () => concat  [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6];

(**** Problem G ****)

fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false;

val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit Z"
  (fn () => isDigit #"Z")
  false;

val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit A"
  (fn () => isDigit #"A")
  false;

val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit 0"
  (fn () => isDigit #"0")
  true;

(**** Problem H ****)

fun isAlpha c = (Char.ord (c) >= Char.ord(#"a") andalso Char.ord(c) <=
	Char.ord(#"z")) orelse (Char.ord (c) >= Char.ord(#"A") andalso Char.ord(c)
	<= Char.ord(#"Z"));

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha a"
  (fn () => isAlpha #"a")
  true;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha g"
  (fn () => isAlpha #"g")
  true;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha z"
  (fn () => isAlpha #"z")
  true;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha K"
  (fn () => isAlpha #"K")
  true;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha Z"
  (fn () => isAlpha #"Z")
  true;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha A"
  (fn () => isAlpha #"A")
  true;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha *"
  (fn () => isAlpha #"*")
  false;

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 9"
  (fn () => isAlpha #"9")
  false;

(**** Problem I ****)

fun svgCircle (cx, cy, r, fill) = (
let
	val cx_str : string = Int.toString cx;
	val cy_str : string = Int.toString cy;
	val r_str : string = Int.toString r;
in
	"<circle cx=\"" ^ cx_str ^ "\" cy=\"" ^ cy_str ^ "\" r=\"" ^ r_str ^ "\" fill=\"" ^ fill ^ "\" />"
end
);

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

(**** Problem J ****)

fun partitionByValue p v [] = []
  | partitionByValue p v (x::xs) = (if p x = v then x :: partitionByValue p v xs
									else partitionByValue p v xs)

(* Could probably also do this with folding, but this way feels a bit more
* resusable. *)
fun partition p x = (partitionByValue p true x, partitionByValue p false x);

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
