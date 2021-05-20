open Go2b
open Syntax

let m_incr =
  (* compute var x in [x<=n || goto l] with l:return x+1 *)
  MCompute (
      ["x"],
      Par [Assign ("x", AVar "n"); Goto "l" ],
      ["l", Return (APrimApp ("+", [AVar "x"; AConst (CInt 1)]))]
    )
                                  
let p = {
    decls = [ MDecl ("incr", (["n"], m_incr)); ];
    entry = ("incr", [CInt 2])
  }

let _ = 
  let v, t = Sem.eval_prog p in
  Printf.printf "result=%s at t=%d\n" (Sem.string_of_val v) t

    
