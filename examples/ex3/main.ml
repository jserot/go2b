open Go2b
open Syntax

let fact = 
  let_ "Fact"
    (func ["n"] 
      (labels 
        [ "f", if_ (prim "<=" [var "n"; int 0])
                      (a_ (var "acc"))
                      (par [ "acc" <= (prim "*" [var "n";var "acc"]) ;  
                             "n" <= (prim "-" [var "n"; int 1]);
                              goto "f" ]) ]
        (par ["acc" <= (int 1) ; goto "f"])))
                             
let p = {
    decls = [ fact ];
    entry = sync ["x", app "Fact" [int 5];
                  "y", app "Fact" [int 8]] 
              (a_ (prim "+" [var "x"; var "y"]))
  }

 (* fact(5) + fact(8) ~> 40440 *)

let _ = 
  Printf.printf "------\n%s\n-------\n" (string_of_program p);
  let v, t = Sem.eval_prog p in
  Printf.printf "result=%s at t=%d\n" (Sem.string_of_val v) t
