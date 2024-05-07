(* funzione simpleMap -> applica una funzione alla lista *)
fun simpleMap(_,nil) =nil
| simpleMap(f,x::xs) = f(x) :: simpleMap(f,xs);
(* funzione reduce -> Applica una generica F alle coppie della lista *)
exception EmptyList;
fun reduce (F,nil) = raise EmptyList
| reduce (F,[a]) = a
| reduce (F,x::xs) = F(x, reduce(F,xs));
reduce(op +,[1,2,3]);
(* funzione filter -> concatena gli elementi che soddisfano una codizione booleana *)
fun filter (P,nil) = nil
| filter (P,x::xs) =
if P(x) then x::filter(P,xs) else filter (P,xs);

fun square (x:real) = x*x;
fun variance (L) =
let
    val n = real(length(L))
in
    reduce (op +,simpleMap(square,L))/n - square
    (reduce(op +,L)/n)
end;
variance([1.0,2.0,3.0,4.4,56.2,67.4,78.2,8.4,9.5]);
(* Exercise L8.1 *)
fun allPositive(nil) = nil
| allPositive(x::tl) = if x < 0.0 then 0.0 :: allPositive(tl) else x::allPositive(tl); 
allPositive[1.0,2.0,3.0,~3.0,~1.0,5.0];
(* Exercise L8.2 *)
val l = [1.1,2.2,4.4,3.3];
reduce(fn (x,y) => if x > y then x else y,l);
(* Exercise L8.3 *)
val l =[1.1,~1.2,~1.3,1.4];
filter(fn x => if x>=0.0 then true else false, l);


(* Funzioni Curried  *)
(* non Curried *)
fun exponent1 (x,0) = 1.0
| exponent1 (x,y) = x * exponent1 (x,y-1);
(* Curried *)
 fun exponent2 x 0 = 1.0
| exponent2 x y = x * exponent2 x (y-1);
(* Exercise L8.5 *)
fun sum1 x = x+1;
fun mul2 x = x+22;
fun pow x = x*x;
fun applyLst  nil _ = nil
| applyLst (f::lstF) x = f(x) :: (applyLst lstF x);
applyLst[sum1,mul2,pow] 2;
(* Exercise L8.6 *)
fun curry f x1 x2 x3 = f(x1,x2,x3);
val g = curry(fn(x,y,z)=> x*y*z);
g 1 2 3;
(* Funzioni Build - in map, foldr, foldr tutte curried *)
(* foldr f c [] -> applica la funzione f alla lista partendo da destra, il valore di c serve per capire che valore deve partire   *)
(* foldl f c [] -> fa la stessa cosa partendo da sinstra  *)
fun f(x,y) = x*y;
foldr f 1 [2,1,4];
(* Exercise L8.7 *)
fun toReal nil = nil
|   toReal (lst) = map (fn (x) => real(x)) lst;
toReal([1,2,3,4]);
(* Exercise L8.8 *)
exception noList of int;
fun logicalAND (nil) = raise noList(0); 
|   logicalAND (lst) = foldr(fn (x,tl) => x andalso tl) true lst;    
logicalAND([true,true,true]);
(* Exercise L8.8 *)
