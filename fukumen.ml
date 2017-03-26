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

let getc comb chr =
  let chr' = String.of_char chr in
  let rec find i =
    match (List.nth comb i) with
    | None -> raise Not_found
    | Some (c, n, _) -> if chr' = c then n else find (i + 1)
  in find 0

let num word comb =
  let getc' = getc comb in
  let len = String.length word in
  let seq = String.rev word in
  let rec aux acc i =
    if i < len
    then aux (acc + (Int.pow 10 i) * (getc' (String.nget seq i))) (i + 1)
    else acc
  in aux 0 0

let () =
  (* enum [("H", 1, (1, 3)); ("A", 0, (0, 9)); ("C", 0, (0, 9)); ("K", 0, (0, 9)); ("E", 0, (0, 9)); ("R", 0, (0, 9))]; *)
  let n = num "CBA" [("A", 1, (0, 1)); ("B", 2, (0, 2)); ("C", 3, (0, 3))] in
  printf "n=%d\n" n;

  (* let data = [("A", 1, (1, 2)); ("B", 0, (0, 1)); ("C", 0, (0, 1))] in *)
  (* let data = [("H", 0); ("A", 0); ("C", 0); ("K", 0); ("E", 0); ("R", 0)] in *)
