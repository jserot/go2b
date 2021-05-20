type ident = string
type var = string
type func_var = string
           
type const =
  | CInt of int
  | CBool of bool
           
type atom =
  | AConst of const
  | AVar of var
  | APrimApp of ident * atom list

type label = string
       
and block =
  | Atom of atom
  | Apply of func_var * atom list
  | Cond of atom * block * block
  | Match of atom * (const * block) list
  | Sync of (var * block) list * block (* sync x1=B1 and ... and xn=Bn in B *)
  | Labels of (label * block) list * block
  | Par of instr list
           
and instr =
  | Assign of var * atom  
  | Goto of label

and func = var list * block  (* fun (x1, ..., xn) -> B *)

type global_decl =
  | FunDecl of ident * func
             
type program = {
    decls: global_decl list;
    entry: block
  }

let if_ a b b' = Cond(a,b,b') 
let app f args = Apply (f,args)  
let int n = AConst (CInt n)
let bool b = AConst (CBool b)
let var b = AVar b
let a_ a = Atom a
let prim p args =APrimApp(p,args)
let goto l = Goto l
let sync bs b = Sync (bs,b)
let (<=) x a = Assign (x,a) 
let par is = Par is
let labels bs b = Labels (bs,b)
let func xs b = (xs,b)
let let_ x f = FunDecl (x,f)

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

let rec string_of_block b = match b with 
  | Atom a -> string_of_atom a
  | Apply (f,es) -> 
     Printf.sprintf "%s(%s)"
       f (Misc.string_of_list string_of_atom "," es)
  | Cond (a, b, b') -> 
      Printf.sprintf "if %s then %s else %s" 
        (string_of_atom a) 
        (string_of_block b) 
        (string_of_block b')
  | Match (a, cases) -> 
      Printf.sprintf "match %s with %s" 
        (string_of_atom a) 
        (string_of_cases cases)
  | Sync (bs, b) -> 
      Printf.sprintf "sync %s in %s" 
        (string_of_bbindings bs) 
        (string_of_block b)
  | Labels (bs,b) -> 
      Printf.sprintf "labels %s in %s" 
        (Misc.string_of_list string_of_lblock "\n" bs)
        (string_of_block b)
  | Par is -> 
      "[" ^ Misc.string_of_list string_of_instr " || " is ^ "]"

and string_of_lblock (l,b) = Printf.sprintf "%s: %s\n" l (string_of_block b)

and string_of_instr i = match i with
  | Assign (v,a) -> Printf.sprintf "%s <= %s" v (string_of_atom a)
  | Goto l -> "goto " ^ string_of_label l

and string_of_cases cs = Misc.string_of_list string_of_case "| " cs

and string_of_case (c,b) = string_of_const c ^ " -> "  ^ string_of_block b

and string_of_bbindings bs = Misc.string_of_list string_of_bbinding " and " bs

and string_of_bbinding (x,b) = x ^ "=" ^ string_of_block b
                                
and string_of_func (xs,b) = 
  Printf.sprintf "fun %s ->\n%s" (string_of_vars xs) (string_of_block b)

let string_of_global_decl = function
  | FunDecl (id, f) -> Printf.sprintf "let %s = %s" id (string_of_func f)
           
let string_of_program p =
  Misc.string_of_list string_of_global_decl "\n" p.decls
  ^ string_of_block p.entry

                              
