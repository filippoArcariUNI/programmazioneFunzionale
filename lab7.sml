(* per sollevare delle eccezioni si usa la parola chiave raise <nomeEccezione> , per dichiararle si usa la parola  exeption <identificator> of <type> *)
(* per gestire le eccezioni si usa: <expression> handle <match> *)

(* Exercise L7.1 *)

exception shortList of int;
fun returnThirdHelper(nil) =  raise shortList(0)
| returnThirdHelper[x] = raise shortList(1)
| returnThirdHelper[x,y] = raise shortList(2)
| returnThirdHelper(x::y::tl) = hd(tl);

fun returnThird(lst) = returnThirdHelper(lst) handle shortList(n) => (print("Lista Corta, contene solo: "); print(Int.toString(n)); print(" elementi\n");0);

returnThird([3,4]);

(* Exercise L7.2 *)
exception Negative of int;
fun factHepler 0 =1
|factHepler n = if (n<0) then raise Negative(n) else n*factHepler(n-1);
fun fact(n) = factHepler(n) handle Negative(n) => (print("Inserire un numero >= 0 \n"); 0);
fact(~5);

(* POLIMORFISMO  *)
(* le funzioni con tipi diversi ML le gestisce con il tipo 'a, tipo polimofro che può assumere altri tipi *)
(* Alcuni operatori ,come (+,-,^,@,ecc.) permettono di fare inferenza di tipo. Gli operatori sulle tuple e sulle liste non lo permettono *)
(* il tipo ''a viene utlizzato per gli operatori che ammettono le uguaglianze, tutti tranne i reali  *)

(* Exercise L7.5 *)
fun f(x,y,z) = if y=y then [z(x),y] else nil;
fun f(x,y,z) = if z=0 then [x,y] else nil;
(* Exercise L7.6 *)
fun f(x,y,z) = z::x;
fun f(x,y,z) = let
  val(a,b) =x
in  
    (y::a,z::b)
end;

(* FUNZIONI di ORDINE SUPERIORE *)
(* funzioni che prendono come parametro altre funzioni *)
fun pow(n) = n * n;
fun tabulate(n,incremento,0, func) = print("\n")
 | tabulate(n,incremento,iterazioni, func) = let
   val newVal = n+incremento
 in
    print(" |  " ^ Int.toString(n) ^ " | " ^ Int.toString(func(n))^"   |\n");
   tabulate(newVal,incremento,iterazioni-1,func)
 end;                                 
tabulate(1,1,10,pow);



