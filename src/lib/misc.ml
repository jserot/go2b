let update_assoc env (k,v) =
  let rec scan l = match l with
    | [] -> [k,v]
    | (k',v')::bs -> if k=k' then (k,v)::bs else (k',v')::scan bs in
  scan env

let string_of_list f sep l =
  let rec h = function
      [] -> ""
    | [x] -> f x
    | x::xs -> f x ^ sep ^ h xs in
  h l

let string_of_list' f lb rb sep l = match l with
    [] -> lb ^ rb
  | [x] -> f x
  | _ -> lb ^ string_of_list f sep l ^ rb
