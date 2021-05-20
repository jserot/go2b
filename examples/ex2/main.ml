open Go2b
open Syntax

let m_fact =
  MCompute (
      ["acc"; "n"],
      Par [
            Assign ("acc", AConst (CInt 1)); 
            Assign ("n", AVar "N");
            Goto "f"
          ],
      ["f", Cond (APrimApp ("=", [AVar "n"; AConst (CInt 0)]),
                     Return (AVar "acc"),
                     Par [Assign ("acc", APrimApp ("*", [AVar "acc"; AVar "n"]));
                          Assign ("n", APrimApp ("-", [AVar "n"; AConst (CInt 1)]))])]
    )
                                  
let p = {
    decls = [
      MDecl ("fact", (["N"], m_fact));
    ];
    entry = ("fact", [CInt 5])
  }

let _ = 
  Printf.printf "------\n%s\n-------\n" (string_of_program p);
  let v, t = Sem.eval_prog p in
  Printf.printf "result=%s at t=%d\n" (Sem.string_of_val v) t
