open Core.Std

(* HACKER *)
(* ENERGY *)

let print list =
  let rec aux acc = function
    | [] -> print_endline (acc ^ "[EOS]")
    | (c, n, (s, e)) :: rest -> aux (acc ^ (sprintf "[%s: %d (%d~%d)] -> " c n s e)) rest
  in aux "" list

let next list' =
  let list = List.rev list' in
  let rec loop dn ls =
    match ls with
    | [] -> dn
    | (c, n, (s, e)) :: rest ->
      if n = e
      then loop (List.append dn [(c, s, (s, e))]) rest (* carry over *)
      else loop (List.append (List.append dn [(c, n + 1, (s, e))]) rest) [] (* break *)
  in List.rev (loop [] list)

let enum init =
  let rec loop ls =
    print ls;
    let ls' = next ls in
    if init = ls' then () else loop ls'
  in loop init

let () =
  enum [("A", 0, (0, 9)); ("B", 0, (0, 9)); ("C", 0, (0, 9))];

  (* let data = [("A", 1, (1, 2)); ("B", 0, (0, 1)); ("C", 0, (0, 1))] in *)
  (* let data = [("H", 0); ("A", 0); ("C", 0); ("K", 0); ("E", 0); ("R", 0)] in *)
