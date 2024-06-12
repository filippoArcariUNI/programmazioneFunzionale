(* Esercizio 2 *)
fun  check nil stringa = true
| check pattern nil = false
| check (p::ptl) (s::stl) = (p=s andalso check ptl stl) orelse check (p::ptl) stl
fun isSubstring lstpattern lstStringa = check(explode(lstpattern))(explode(lstStringa));
isSubstring "" "ciao";
(* Esercizio 3 *)
