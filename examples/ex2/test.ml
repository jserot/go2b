#use "topfind";;
#directory "../../_build/default/src";;
#directory "../../_build/default/src/.go2b.objs/byte";;
#load "go2b.cma";;

open Go2b
open Syntax

let m_fact =
  MCompute {
      mvar = ["acc"; "n"];
      minit =
        Par [
            Assign ("acc", AConst (CInt 1)); 
            Assign ("n", AVar "N");
            Goto "f"
          ];
      mblocks = [
          "f", Cond (APrimApp ("=", [AVar "n"; AConst (CInt 0)]),
                     Return (AVar "acc"),
                     Par [Assign ("acc", APrimApp ("*", [AVar "acc"; AVar "n"]));
                          Assign ("n", APrimApp ("-", [AVar "n"; AConst (CInt 1)]))])
          ]
    }
                                  
let m_main =
  MCompute {
      mvar = [];
      minit =
        Join (
          [ MInst ("fact", [AVar "x"]); MInst ("fact", [APrimApp ("+", [AVar "x"; AConst (CInt 1)])]) ],
          [ "r1"; "r2"],
          Return (APrimApp ("+", [AVar "r1"; AVar "r2"])));
      mblocks = []
    }
      
let p = {
    decls = [
      MDecl ("fact", (["N"], m_fact));
      MDecl ("main", (["x"], m_main));
    ];
    entry = ("main", [VInt 3])
  }

let v, t = Sem.eval_program p   
