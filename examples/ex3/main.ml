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
                                  
let m_main =
  MCompute (
      [],
      Sync (
          [ "r1", MInst ("fact", [AVar "x"]);
            "r2", MInst ("fact", [APrimApp ("+", [AVar "x"; AConst (CInt 1)])]) ],
          Return (APrimApp ("+", [AVar "r1"; AVar "r2"]))),
      []
    )

let p = {
    decls = [
      MDecl ("fact", (["N"], m_fact));
      MDecl ("main", (["x"], m_main));
    ];
    entry = ("main", [CInt 4])
  }

let _ = 
  let v, t = Sem.eval_prog p in
  Printf.printf "result=%s at t=%d\n" (Sem.string_of_val v) t
