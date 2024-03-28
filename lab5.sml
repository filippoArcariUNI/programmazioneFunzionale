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
(* Exercise L5.4  TODO -> domandare come è possibile senza i pattern??, per forza arriverà al punto che la lista è vuota*)
fun split(lst) =
  let
    fun helper(nil, A ,B) = (A , B) 
    | helper (a::nil,A,B) = helper(nil,a::A,B)
    | helper (a :: b :: rest, A, B) = helper(rest, a::A , b::B)
  in
    helper(lst, [], [])
  end;

split([12,3,45,5,6,8]);

(* Exercise L5.6 *)
fun maxList(lst) = 
let
  fun maxHelper(nil,max) = max
  | maxHelper(x::tl,max) = if x > max then maxHelper(tl,x) else maxHelper(tl,max)
in
  maxHelper(lst,0.0)
end;
maxList([12.3,3.14,4.25,45.4,5.6,6.5,8.7]);

(* Exercise L5.7 *)
fun doubleExp(x:real,i)=
let
  fun exp(0,exp)= 1.0
  | exp(base,exp) =base *exp(exp-1)
  
in
  exp(x,exp(2.0,i-1)) * exp(x,exp(2.0,i-1))
end;

(* doubleExp(3); *)
