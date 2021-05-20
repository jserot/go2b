type ident = string
type var = string
           
type const =
  | CInt of int
  | CBool of bool
           
type atom =
  | AConst of const
  | AVar of var
  | APrimApp of ident * atom list

type label = string
              
and machine =
  | MInst of ident * atom list   (* m(a1, ..., an) *)
  | MCompute of var list * instr * block list (* compute var (x1,...,xn) in s with b1, ..., bn *)

and block = label * instr
           
and instr =
  | Assign of var * atom
  | Par of instr list
  | Cond of atom * instr * instr
  (* | Match of atom * (const * instr) list *)
  | Goto of label
  | Return of atom
  | Let of var * mdecl * instr  (* let m = fun (x1, ..., xn) -> M in s *)
  | Sync of (var * machine) list * instr (* sync x1=M1 and ... and xn=Mn in s *)
  | Nil (* Nothing : associated to terminal block *)

and mdecl = var list * machine  (* fun (x1, ..., xn) -> M *)

type global_decl =
  | MDecl of ident * mdecl
             
type program = {
    decls: global_decl list;
    entry: ident * const list
  }

let new_label =
  let cnt = ref 0 in
  fun () -> incr cnt; "l" ^ string_of_int !cnt
