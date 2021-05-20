type env = (Syntax.ident * semval) list

and semval =
  | Value of Syntax.const
  | Clos of Syntax.func * env
  | OLabel of Syntax.label
  | SNone
let string_of_object = function
  | Value v -> Syntax.string_of_const v
  | Clos (_,_) -> "<closure>"
  | OLabel l -> l
  | SNone -> invalid_arg "string_of_object"

let string_of_val = function
| Value _ as v -> string_of_object v
| _ -> invalid_arg "string_of_val"     
let encode_int n =
    Value(Syntax.CInt n)

let decode_int = function
  | Syntax.(Value(CInt n)) -> n
  | _ -> failwith "Builtins.decode_int" (* should not happen *)

let encode_bool b =
    Syntax.(Value(CBool b))

let decode_bool = function
  | Value(Syntax.CInt b) -> b
  | _ -> failwith "Builtins.decode_bool" (* should not happen *)

let decode_clos = function
  | Clos (f,r) -> (f,r)
  | _ -> failwith "Builtins.decode_clos" (* should not happen *)

let encode_clos l =
    OLabel l

let decode_label = function
  | OLabel l -> l
  | _ -> failwith "Builtins.decode_label" (* should not happen *)

let prim1 encode op decode =
  function v -> encode (op (decode v))
let prim2 encode op decode1 decode2 =
  function
   | [v1;v2] ->
       encode (op (decode1 v1) (decode2 v2))
   | _ -> failwith "Builtins.prim2"

let primitives = [
    (* Id, arity, static value *)
    "+",  (2, prim2 encode_int  ( + ) decode_int decode_int);
    "-",  (2, prim2 encode_int  ( - ) decode_int decode_int);
    "*",  (2, prim2 encode_int  ( * ) decode_int decode_int);
    "/",  (2, prim2 encode_int  ( / ) decode_int decode_int);
    "=",  (2, prim2 encode_bool ( = ) decode_int decode_int);
    "!=", (2, prim2 encode_bool ( <> ) decode_int decode_int);
    "<",  (2, prim2 encode_bool ( < ) decode_int decode_int);
    ">",  (2, prim2 encode_bool ( > ) decode_int decode_int);
    "<=",  (2, prim2 encode_bool ( <= ) decode_int decode_int);
  ]

let pc_register = "pc"
let res_register = "res"


exception Unknown_id of Syntax.ident
exception Unknown_prim of Syntax.ident
exception Illegal_prim_app of Syntax.ident * Syntax.atom list
                      
let lookup v r =
  try List.assoc v r
  with Not_found -> raise (Unknown_id v)

let lookup_prim p =
  try List.assoc p primitives 
  with Not_found -> raise (Unknown_id p)

(* r |-atom a => v *)
                  
let rec eval_atom r a =
  let open Syntax in
  match a with
  | AConst (CInt i) -> Value (CInt i)
  | AConst (CBool b) -> Value (CBool b)
  | AVar x -> lookup x r
  | APrimApp (p, args) ->
     let n, op = lookup_prim p in
     let vs = List.map (eval_atom r) args in
     if List.length args = n
     then op vs
     else raise (Illegal_prim_app (p,args))
                                        

exception Match_fail of Syntax.const
     
(* r |-stat b => p *)
let eval_instr r s = 
  let open Syntax in
  match s with 
  | Goto l -> ("pc",OLabel l)
  | Assign (x,a) -> (x,eval_atom r a)

(* L,r |-block b => p,t *)
let rec eval_block ls r b =
  let open Syntax in
  match b with   
  | Atom a -> 
    let v = eval_atom r a in
    let r' = Misc.update_assoc r (res_register,v) in
    r',0 
  | Apply (x,args) -> 
    let vs = List.map (eval_atom r) args in
    
    (match lookup x r with
     | Clos((xs,b),r') ->
        let bs = List.combine xs vs in
        let r'' = List.fold_left Misc.update_assoc r' bs in
        let v,t = eval_return r'' b in
        let rr = Misc.update_assoc r (res_register,v) in
        rr,t
    | _ -> failwith "Illegal app") 
  | Cond (a, b, b') ->
     begin match eval_atom r a with
     | Value (CBool true) -> eval_block ls r b
     | Value (CBool false) -> eval_block ls r b'
     | _ -> failwith "Illegal cond"
     end
  | Match (a, cases) ->
     let c =
       begin match eval_atom r a  with
             | Value c -> c
             | _ -> failwith "Illegal match"
       end in
     let b =
       try List.assoc c cases
       with Not_found -> raise (Match_fail c) in
     eval_block ls r b
  | Labels (bs,b) ->
      let ls' = bs @ ls in
      eval_block ls' r b
  | Sync (bs,b) ->
    let bs = List.map (fun (x,b) -> x, eval_return r b) bs in (* Note: left-to-right evaluation order here *)
    let t = List.fold_left (fun t (_,(_,t')) -> max t t') 0 bs in
    let bs = List.map (fun (x,(v,_)) -> (x,v)) bs in
    let r' = List.fold_left Misc.update_assoc r bs in
    let r'',t' = eval_block ls r' b in
    r'',(t+t')
  | Par ss ->
     let r0 = Misc.update_assoc r (pc_register,SNone) in
     let r1 = Misc.update_assoc r0 (res_register,SNone) in
     let bs = List.map (eval_instr r1) ss in (* Note: left-to-right evaluation order here *)
     let r' = List.fold_left Misc.update_assoc r1 bs in 
     (match lookup pc_register r' with
            | SNone -> failwith "Illegal Par" (* goto mandatory *)
            | OLabel _ as l -> 
                let b = lookup (decode_label l) ls in 
                let (r'',t) = eval_block ls r' b in (* c'est bien [r'] *)
                r'',(1+t)
            | _ -> assert false)

(* r |-return b => v,t *)
and eval_return r b =
  let r',t = eval_block [] r b in
  match lookup res_register r' with
  | Clos _ -> failwith "Illegal return"
  | v -> v,t

(* r |-prog => v, t *)

let eval_fun_decl r decl =   (* Global Let *)
  let open Syntax in
  match decl with
  | FunDecl (id, f) -> (id, Clos (f,r))::r

let eval_global_decls p decls =
  List.fold_left eval_fun_decl p decls
  
let eval_prog p =
  let open Syntax in
  let initial_env = [] in  
  let r = eval_global_decls initial_env p.decls in
  eval_return r p.entry
