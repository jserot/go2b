#use "topfind";;
#directory "../../_build/default/src/lib";;
#directory "../../_build/default/src/lib/.go2b.objs/byte";;
#load "go2b.cma";;

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
    entry = ("fact", [CInt 3])
  }

let v, t = Sem.eval_prog p   
