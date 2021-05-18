type genv = (Syntax.ident * mclos) list  (* Omega *)

and mclos = Clos of Syntax.mdecl * genv        

type semval =
  | Int of int
  | Bool of bool
  | Unknown

let encode_int n =
    Int n
let decode_int = function
  | Int n -> n
  | _ -> failwith "Builtins.decode_int" (* should not happen *)
let encode_bool b =
    Bool b
let decode_bool = function
  | Bool b -> b
  | _ -> failwith "Builtins.decode bool" (* should not happen *)

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
  ]

type frame = { (* bloc d'activation *)
    lvars: (Syntax.var * semval) list;
    pc: Syntax.label;
    stopc: Syntax.label;
    res: semval;
    time: int
  }

exception Unknown_id of Syntax.ident
exception Unknown_prim of Syntax.ident
exception Illegal_prim_app of Syntax.ident * Syntax.atom list
                      
let lookup v env =
  try List.assoc v env
  with Not_found -> raise (Unknown_id v)

let lookup_prim p =
  try List.assoc p primitives 
  with Not_found -> raise (Unknown_id p)

(* F |-atom a => v *)
                  
let rec eval_atom f a =
  let open Syntax in
  match a with
  | AConst (CInt i) -> Int i
  | AConst (CBool b) -> Bool b
  | AVar x -> lookup x f.lvars
  | APrimApp (p, args) ->
     let n, op = lookup_prim p in
     let vs = List.map (eval_atom f) args in
     if List.length args = n
     then op vs
     else raise (Illegal_prim_app (p,args))
                                        
(* O,F |-stat s => F' *)

exception Unsupported_instr of Syntax.instr
                             
let rec eval_instr o f s = 
  let open Syntax in
  match s with 
  | Goto l' when f.pc = "" ->
      { f with pc = l' }   (* "" = none *)
  | Return a when f.pc = "" ->
     let v = eval_atom f a in
     { f with res = v; pc = f.stopc }
  | Assign (x,a) when List.mem_assoc x f.lvars ->
     let v = eval_atom f a in
     { f with lvars = Misc.update_assoc f.lvars (x,v) }
  | Cond (a, s, s') ->
     begin match eval_atom f a with
     | Bool true -> eval_instr o f s
     | Bool false -> eval_instr o f s'
     | _ -> failwith "Illegal cond"
     end
  | Let (m, d, s) ->
     let o' = Misc.update_assoc o (m, Clos (d,o)) in
     eval_instr o' f s
  | Par ss ->
     List.fold_left (eval_instr o) f ss  (* Note: left-to-right evaluation order here *)
  (* TODO : Match and ... MORE IMPORTANTLY: Join ! *)
  | _ ->
     raise (Unsupported_instr s)
     
(* O,F |-block b => v, t *)

let eval_block o f s =
  let _ = o,s in f  (* TO BE FIXED *)
  (* let f' = eval_instr o { f with pc = "" } s in
   * if f'.pc = "" then (\* Continue *\)
   *   { f with  *)

(* O,F |-blocks b1, ..., bn => v, t *)

exception Unknown_label of Syntax.label
                         
let lookup_block l bs =
  try List.assoc l bs
  with Not_found -> raise (Unknown_label l)

let rec eval_blocks o f bs = 
  if f.pc = f.stopc then    (* End *)
    f.res, f.time
  else                      (* Blocks *)
    let b = lookup_block f.pc bs in
    let f' = eval_block o f b in
    eval_blocks o f' bs

(* O |-machine M => v, t *)

let rec eval_machine o m =
  let open Syntax in
  match m with
  | MInst (id, args) -> (* Instance *)
     begin match lookup id o with
     | Clos ((xs, MCompute (ys, s, bs)), o') ->
        let l = Syntax.new_label () in
        let m' = MCompute (ys, Par (List.map2 (fun x a -> Assign (x,a)) xs args @ [Goto l]),  (l,s)::bs) in
        eval_machine o' m'
     | _ -> failwith "eval_machine"
     end
  | MCompute (ys, s, bs) -> (* Compute *)
     let l = new_label () in
     let l' = new_label () in
     let fi = { (* F_init *)
         pc = l;
         stopc = l';
         time = 0;
         lvars = List.map (fun y -> y, Unknown) ys;
         res = Unknown } in
     eval_blocks o fi ([l,s] @ bs @ [l', Nil])
      
(* O |-prog => v, t *)

let eval_global_decl o decl =   (* Global Let *)
  let open Syntax in
  match decl with
  | MDecl (id, md) -> (id, Clos (md,o))::o 

let eval_global_decls o decls =
  List.fold_left eval_global_decl o decls
  
let eval_prog p =
  let open Syntax in
  let initial_env = [] in  
  let o = eval_global_decls initial_env p.decls in
  let id, args = p.entry in
  eval_machine o (MInst (id, List.map (fun c -> AConst c) args))   (* Main *)
