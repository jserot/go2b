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
  | Match of atom * (const * instr) list
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

(* Printing *)

let string_of_label l = l
                      
let string_of_const = function
  | CInt i -> string_of_int i
  | CBool b -> string_of_bool b

let string_of_vars xs = Misc.string_of_list' Fun.id "(" ")" "," xs
                          
let rec string_of_atom a = match a with
  | AConst c -> string_of_const c
  | AVar v -> v
  | APrimApp (p,args) -> Printf.sprintf "%s(%s)" p (Misc.string_of_list string_of_atom "," args)
                       
let rec string_of_instr i = match i with
  | Assign (v,a) -> Printf.sprintf "%s <= %s" v (string_of_atom a)
  | Par is -> "[" ^ Misc.string_of_list string_of_instr " || " is ^ "]"
  | Cond (a, i, i') -> Printf.sprintf "if %s then %s else %s" (string_of_atom a) (string_of_instr i) (string_of_instr i')
  | Match (a, cases) -> Printf.sprintf "match %s with %s" (string_of_atom a) (string_of_cases cases)
  | Goto l -> "goto " ^ string_of_label l
  | Return a -> "return " ^ string_of_atom a
  | Let (id,md,i) -> Printf.sprintf "let %s = %s in %s" id (string_of_mdecl md) (string_of_instr i)
  | Sync (bs, i) -> Printf.sprintf "sync %s in %s" (string_of_mbindings bs) (string_of_instr i)
  | Nil -> ""

and string_of_cases cs = Misc.string_of_list string_of_case "| " cs

and string_of_case (c,i) = string_of_const c ^ " -> "  ^ string_of_instr i

and string_of_mbindings mbs = Misc.string_of_list string_of_mbinding " and " mbs

and string_of_mbinding (x,m) = x ^ "=" ^ string_of_machine m
                             
and string_of_block (l,i) = Printf.sprintf "%s: %s\n" l (string_of_instr i)
                          
and string_of_machine m = match m with
  | MInst (id, args) -> Printf.sprintf "%s(%s)\n" id (Misc.string_of_list string_of_atom "," args)
  | MCompute (xs, s, bs) ->
     Printf.sprintf "compute\nvar %s in\n%s\nwith\n%send\n"
       (Misc.string_of_list Fun.id "," xs)
       (string_of_instr s)
       (Misc.string_of_list string_of_block "\n" bs)
  
and string_of_mdecl (xs,m) = 
  Printf.sprintf "fun %s ->\n%s" (string_of_vars xs) (string_of_machine m)

let string_of_global_decl = function
  | MDecl (id, md) -> Printf.sprintf "let %s = %s" id (string_of_mdecl md)
           
let string_of_program p =
  let id, args = p.entry in
  Misc.string_of_list string_of_global_decl "\n" p.decls
  ^ "\n" ^ id ^ " " ^ Misc.string_of_list' string_of_const "(" ")" "," args
                              
                              
