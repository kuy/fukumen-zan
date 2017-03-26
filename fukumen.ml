open Core.Std

(* HACKER *)
(* ENERGY *)

let print list =
  let rec aux acc = function
    | [] -> print_endline (acc ^ "[EOS]")
    | (c, n) :: rest -> aux (acc ^ (sprintf "[%s: %d] -> " c n)) rest
  in aux "" list

let next list' =
  let list = List.rev list' in
  match list with
  | [] -> []
  | (c, n) :: rest -> List.rev ((c, n + 1) :: rest)

let () =
  let data = [("H", 0); ("A", 0); ("C", 0); ("K", 0); ("E", 0); ("R", 0)] in
  print data;
  let data' = next data in
  print data';
  let data'' = next data' in
  print data''
  (* assert ((expression "1+2" 0) = (Some ["1"; "+"; "2"], 3)); *)
