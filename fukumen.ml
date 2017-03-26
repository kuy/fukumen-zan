open Core.Std

(* HACKER *)
(* ENERGY *)

let print comb =
  let rec aux acc = function
    | [] -> print_endline (acc ^ "[EOS]")
    | (c, n, (s, e)) :: rest -> aux (acc ^ (sprintf "[%s: %d (%d~%d)] -> " c n s e)) rest
  in aux "" comb

let next comb' =
  let comb = List.rev comb' in
  let rec loop dn cmb =
    match cmb with
    | [] -> dn
    | (c, n, (s, e)) :: rest ->
      if n = e
      then loop (List.append dn [(c, s, (s, e))]) rest (* carry over *)
      else loop (List.append (List.append dn [(c, n + 1, (s, e))]) rest) [] (* break *)
  in List.rev (loop [] comb)

let getc comb chr =
  let chr' = String.of_char chr in
  let rec find i =
    match (List.nth comb i) with
    | None -> raise Not_found
    | Some (c, n, _) -> if chr' = c then n else find (i + 1)
  in find 0

let num comb word =
  let getc' = getc comb in
  let len = String.length word in
  let seq = String.rev word in
  let rec aux acc i =
    if i < len
    then aux (acc + (Int.pow 10 i) * (getc' (String.nget seq i))) (i + 1)
    else acc
  in aux 0 0

let check comb =
  let num_of = num comb in
  let hacker = num_of "HACKER" in
  let energy = num_of "ENERGY" in
  (3 * hacker) = energy

let dup comb =
  comb
  |> List.map ~f:(fun (_, n, _) -> n)
  |> List.contains_dup ~compare:(fun a b -> a - b)

let find init =
  let rec loop cmb =
    if (not (dup cmb)) && check cmb
    then print cmb
    else
      let cmb' = next cmb in
      if init = cmb' then () else loop cmb'
  in loop init

let () =
  find [("H", 1, (1, 3)); ("A", 0, (0, 9)); ("C", 0, (0, 9)); ("K", 0, (0, 9)); ("E", 3, (3, 9)); ("R", 1, (1, 9)); ("N", 0, (0, 9)); ("G", 0, (0, 9)); ("Y", 1, (1, 9))]
