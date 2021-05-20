open Go2b
open Syntax

let f_even = 
  let_ "IsEven"
    (func ["n"] 
      (labels 
        [ "even", if_ (prim "<=" [var "n"; int 0])
                      (a_ (bool true))
                      (par [ "n" <= (prim "-" [var "n"; int 1]) ; 
                                  goto "odd"]) ;
          "odd",  if_ (prim "<=" [var "n"; int 0])
                      (a_ (bool false))
                      (par [ "n" <= (prim "-" [var "n"; int 1]) ; 
                                  goto "even"]) 
        ]
        (par [goto "even"])))
                             
let p = {
    decls = [ f_even ];
    entry = app "IsEven" [int 42]
  }

let _ = 
  Printf.printf "------\n%s\n-------\n" (string_of_program p);
  let v, t = Sem.eval_prog p in
  Printf.printf "result=%s at t=%d\n" (Sem.string_of_val v) t
