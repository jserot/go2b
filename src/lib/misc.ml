let update_assoc env (k,v) =
  let rec scan l = match l with
    | [] -> [k,v]
    | (k',v')::bs -> if k=k' then (k,v)::bs else (k',v')::scan bs in
  scan env
