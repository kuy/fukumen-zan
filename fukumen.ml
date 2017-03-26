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

let () =
  let s0 = [("A", 0, (0, 1)); ("B", 0, (0, 1))] in
  print s0;
  let s1 = next s0 in
  print s1;
  let s2 = next s1 in
  print s2;
  let s3 = next s2 in
  print s3;
  let s4 = next s3 in
  print s4;

  (* let s0 = [("A", 1, (1, 2)); ("B", 0, (0, 1)); ("C", 0, (0, 1))] in *)
  (* let data = [("H", 0); ("A", 0); ("C", 0); ("K", 0); ("E", 0); ("R", 0)] in *)
