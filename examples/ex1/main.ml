open Go2b
open Syntax
                               
let p = {
    decls = [];
    entry = Atom (AConst (CInt 42))
  }

let _ = 
  Printf.printf "------\n%s\n-------\n" (string_of_program p);
  let v, t = Sem.eval_prog p in
  Printf.printf "result=%s at t=%d\n" (Sem.string_of_val v) t

    
