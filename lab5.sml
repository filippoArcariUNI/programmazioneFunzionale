(* Exercise L5.1 *)
fun is_one(n) = case n of
   1 => "One"
 | _ => "anything_else";
is_one(2);
(* Exercise L5.2 *)
fun is_lower_than5(n) = case n<5 of
    true => true
    | _ => false;
is_lower_than5(6);
(* Exercise L5.3 *)
fun thousandthPower(n:real) =
let
  val five = n*n*n*n*n
  val twenty = five * five * five * five 
in
  twenty * twenty * twenty *twenty *twenty
end;
thousandthPower(2.0);
(* Exercise L5.4 *)
fun split(lst) =
let
  val x = hd(lst)
  val tlLst= tl(lst)
in
  x :: split (tlLst)
end;
split([12,3,45,5,6,8]);