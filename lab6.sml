fun fact(0) = 1
| fact (n) = n * fact(n-1);
(* Input e Output *)
(* Output : print(x) string -> unit "stampa una stringa e " *)
fun testZero(0) = print("Zero\n")
| testZero(_) = print("Not Zero\n");
testZero(1);
(* Se volessimo stampare interi o reali ?? 
possiamo utilzzare il metodo toString delle strutture dati come Real,Int, Bool*)
fun printReal(n) = print(Int.toString(n));
printReal(1);
(* per andare a capo : utlizzo una parentesi annidata compound statement, il tipo è quello dell' ultima istruzione *)
fun printReal(n) =(print(Int.toString(n)); print("\n")); 
printReal(1);


(* Exercise L6.1  *)
fun printList(nil) = print("\n")
| printList(x::tl) = (print(Int.toString(x)); print("-"); printList(tl));
printList([1,2,3,4,5]);
(* Exercise L6.2  *)
fun binomial(n,m) = let
  val num= fact(n)
  val den= fact(m) * fact(n-m) 
in
  (print(Int.toString(num)); print("/"); print(Int.toString(den)) ; print("="); print(Int.toString(num  div den));print("\n"))
end;
binomial(6,4);

(* Exercise L6.3  *)
fun printX(0) = print("x")
| printX(n) = let
  fun printer(0)= ()
  | printer(n) = (printer(n-1); print("X"))
  
  fun exp(base ,0) = 1
  | exp (base,n) = base* exp(base,n-1)
 in
  (printer(exp(2,n)); print("\n"))
end;
printX(2);

(* Input : Aprire un file in input TextIO.openIn("pathFile") *)
val myFile=TextIO.openIn("./test.txt");
(* la funzione  TextIO.endOfStream(myFile) ci dice se siamo alla fine del file | Ritorna un booleano
                TextIO.inputN(myFile, n) legge n caratteri dallo stream        | Ritorna un char 
                TextIO.inputLine (infile); legge una riga del file             | Ritorna un <T> option -> se c'è una riga ritorna la riga, altrimenti ritorna None 
                TextIO.closeIn(infile); chiude il file                         | Ritorna il vuoto
                TextIO.input1; legge un carattere                              | Ritorna un char option
                TextIO.lookahead; guarda il carattere dopo senza andare anvanto con lo stream  | Ritorna un char option 
                TextIO.canInput(f,50); guarda se ci sono 50 caratteri nel file | ritorna un int option -> SOME 50 se ci sono 50 caratteri , SOME 10 se ci sono 10' caratteri *)


(* Exercise L6.5 *)
fun read5Cahr() =TextIO.inputN(myFile, 5);
read5Cahr();
fun readLine() =  TextIO.inputLine (myFile);
readLine();
fun look() =  TextIO.lookahead;
look();
fun readFullFile() = TextIO.input(myFile);
readFullFile();
(* Exercise L6.6 *)
val fileLst=TextIO.openIn("./test.txt");

fun getList(filename) = if  TextIO.endOfStream(filename) then nil else let
  fun isDivider (c) = case c of
      #"\n" => true
    | #"\t" => true
    | #" " => true
    | _ => false;

  val c = case TextIO.input1(filename) of
     None => #"\n" 
   | SOME c => c;

  fun getWord(fileName) = if isDivider(c) then "" else  getWord(fileName) ^ str(c);
  val w = getWord(filename);
in
  w::getList(filename)
end;
getList(fileLst);


