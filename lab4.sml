(* x::xs divide la lista nel primo elemento e tutta la sua coda *)
fun reverse nil =nil | reverse(x::xs) =reverse (xs) @ [x];
reverse([1,2,3,4]);
(* as -> da il nome al pattern *)
fun merge(M,nil)=M | merge (nil,L)= L | merge(L as x::xs , M as y::ys) = if x<y then x::merge(xs,M) else y::merge(L,ys); 
(* senza as *)  
fun merge (nil,M) = M| merge (L,nil) = L| merge (x::xs,y::ys) =if x<y then x::merge(xs,y::ys) else y::merge(x::xs,ys);
(* wildcard -> quando non serve dare un nome al mio pattern nel esempio non mi interessa il valore del primo parametro*)
fun comb (_,0) = 1 | comb (n,k) = if k=n then 1 else comb(n-1,k)+comb(n-1,k-1);
fun sumPairs (nil) = 0 | sumPairs((x,y)::zs) = x + y + sumPairs(zs);
(* Exercise L4.2 *)
fun factPattern 0 = 1 |  factPattern(n)= factPattern(n-1)*n;
factPattern(5);
(* Exercise L4.3 *)
fun cycle nil=nil | cycle (x::xt) = xt @ [x] ;
cycle ([1,2,3,4,5]);
(* Exercise L4.4 *)
fun cycle_i (nil,_)=nil
| cycle_i(x,0) = x
| cycle_i(x::xt,i) = cycle_i(xt @ [x],i-1) ;
cycle_i([1,2,3,4,5], 3);
(* Exercise L4.5 *)
fun duplicate(nil) = nil 
| duplicate(x::xt) = x :: x :: duplicate(xt); 
duplicate([1,2,3,4,5]);
(* Exercise L4.6 *)
fun pow(base,0)= 1
| pow(base,exp) = base*pow(base,exp-1);
pow(2,10);
(* Exercise L4.7 *)
fun  max (nil) = 0.0 
| max ([x:real]) = x
| max (x::y::zt) = if x > y then max(x::zt) else max(y::zt);
max([1.0,25.0,72.4,1.2,63.4]);
(* Exercise L4.8 *)
fun flips(nil) = nil
| flips(x) = x
| flips(x::y::z) = y::x::flips(z);
flips([1,2,3,4,5,6,7,8,9]);
(* Exercise L4.9 *)
fun lenght(nil) = 0
| lenght(x::xt) = 1+ lenght(xt); 
fun delete_i_item (nil, i) = nil
| delete_i_item(x)