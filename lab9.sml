(* o ->  compone 2 funzioni f(g(x)) *)
(* E' possibile definire un tipo tramite la parola chiave type *)
(* E' possibile definire un tipo tramite la parola chiave type *)
type signal = int list;
val v = [1,2]: signal;
(* Exercise L9.1 *)
type ('a) setOfSets = 'a list list;
val x = [[1,2,3],[2],[4]]:int setOfSets;
(* Exercise L9.2 *)
type ('a,'b) tripleList = ('a*'a *'b ) list 
(* Exercise L9.2 *)
type ('c,'d) mapping = ('c * 'd) list;
val x = [(2.4,1.5),(3.14,1.7)]: (real,real) mapping;
(* la parola chiave datatype permette la creazione di un tipo *)
datatype fruit = Apple | Pear | Grape;
(* fruit può asusmere SOLO quei valori *)
fun isApple (x) = (x = Apple);
isApple (Pear);
isApple(Apple);
(* isApple (Cherry); -> da erorre perchè "cherry" non sta in fruit *)
(* Si può definire datatype che sono parametrici *)
datatype ('a,'b) element =
P of 'a * 'b |
S of 'a;
(* considerà sia tuple di due elementi che un singolo elemento *)
P("a",1);
P(1.0,2.0);
S(["a","b"]);
fun sumElList (nil) = 0
| sumElList (S(x)::L) = sumElList (L)
| sumElList (P(x,y)::L) = y + sumElList (L);
sumElList [ P("in",6), S("function"),P("as",2)];
(* datatyoe RICORSIVI *)
datatype 'label btree =
Empty |
Node of 'label * 'label btree * 'label btree;
Node ("ML",
    Node ("as", (* sottoalbero di sx e figli *)
        Node ("a",Empty,Empty),
        Node ("in",Empty,Empty)
    ),
    Node ("types",Empty,Empty)  (* sottoalbero di dx senza figli *)
);
(* Signatures and structures *)
(* (structure : signatures = classe : metodi_pubblici)  esempio in oop, *)
structure IntLT = struct
    type t = int
    val lt = (op <)
    val eq = (op =)
end;
(* questa struct può essere geralizzata con *)
signature ORDERED = sig
    type t
    val lt : t * t -> bool
    val eq : t * t -> bool
end;
(* ESEMPIO *)
signature QUEUE =
    sig
    type 'a queue
    exception QueueError
    val empty : 'a queue
    val isEmpty : 'a queue -> bool
    val singleton : 'a -> 'a queue
    val insert : 'a * 'a queue -> 'a queue
    val remove : 'a queue -> 'a * 'a queue
end;
(* Queste sono solo le firme delle funzioni (file Header), per collegare firme alla structure si usa ":>" alla fine della struct che risolve le firme  *)
signature STACK =
    sig
    val empty: 'a list
    val pop: 'a list -> 'a option
    val push: 'a * 'a list -> 'a list
    eqtype 'a stack
end;
structure Stack = struct
    type 'a stack = 'a list
    val empty = []
    val push = op::
    fun pop [] =NONE
    | pop (tos::rest) =SOME tos
end :> STACK;
(* l'implementazione di stack non è visibile all'utilizzatore *)
structure S = Stack;
 S.push (1, S.empty);
(* Exercise L9.4 *)
signature SET =
sig
  type 'a set 
  val emptySet: 'a set 
  val member: ''a -> ''a set -> bool
  val inset : ''a -> ''a set -> ''a set 
  val remove: ''a -> ''a set -> ''a set
end;

(* structure Set =
strucvalt
    type 'a set = 'a list

  val emptySet = nil;
  fun member(_,nil) = false
  | member x y::ys = if x=y then true else member(x,ys)
  fun insert (x, lst) = if member x lst then lst else x::lst 
  fun remove (x, lst) = if member x lst then 
end:> SET; *)