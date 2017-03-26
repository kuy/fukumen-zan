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
  let rec loop dn ls =
    match ls with
    | [] -> dn
    | (c, n) :: rest ->
      if n = 2
      then loop ((c, 0) :: dn) rest (* carry over *)
      else loop (List.append (List.append dn [(c, n + 1)]) rest) [] (* break *)
  in List.rev (loop [] list)

let () =
  let s0 = [("A", 0); ("B", 0); ("C", 0)] in
  print s0;
  let s1 = next s0 in
  print s1;
  let s2 = next s1 in
  print s2;
  let s3 = next s2 in
  print s3;
  let s4 = next s3 in
  print s4;

  (* let data = [("H", 0); ("A", 0); ("C", 0); ("K", 0); ("E", 0); ("R", 0)] in *)
