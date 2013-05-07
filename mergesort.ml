(* This is a recursive mergesort exercise I implemented to study for a CS final I have *)

let print_list (l: int list) =
  List.iter (fun elem -> Printf.printf "%d " elem) l;
  Printf.printf "%s\n" ""
;;

let rec merge (l1: int list) (l2: int list) : int list =
  match l1, l2 with
  | [],[] -> []
  | [], l | l, [] -> l
  | a::ta, b::tb -> if a < b then a::(merge ta l2) else b::(merge l1 tb)
;;

let split (lst: int list) : (int list * int list) = 
  let rec cut_down lst count =
    if count > 0 then
      match lst with
      | [] -> ([], lst)
      | hd::tl -> 
          let f,s = (cut_down tl (count-1)) in
          (hd::f, s)    
    else ([], lst)
  in
  let len = (List.length lst) in
  cut_down lst (len / 2)
;;

let rec mergesort (lst: int list) : int list =
  match lst with
  | [] | _::[] -> lst
  | _ -> 
    let l1,l2 = split lst in
    let res = merge (mergesort l1) (mergesort l2) in
    (*print_list res;*)
    res
;;

assert (mergesort [] = []);;
assert (mergesort [1] = [1]);;
assert (mergesort [2;1] = [1;2]);;
assert (mergesort [2;1;4;1;6;7;2;3;1;5] = [1;1;1;2;2;3;4;5;6;7]);;
assert (mergesort [(-1);2;(-5)] = [(-5);(-1);2]);;
assert (mergesort [1;5;4;3] = [1;3;4;5]);;