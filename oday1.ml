(* AoC 2020, Day1 ocaml code *)

open List

module IntSet = Set.Make(Int)

let rec read_lines f =
  try
    let l = input_line f in
    (int_of_string l) :: read_lines f
  with
    End_of_file -> []

let read_datas () =
  let f = open_in "day1.txt" in
  let ls = read_lines f in
  close_in f;
  ls

let concat_map f ls = concat (map f ls)

let rec enum from two =
  if from > two
  then []
  else from :: enum (from+1) two

let combinations k datas =
  let arr = Array.of_list datas in
  let n = Array.length arr - 1 in
  let rec go start depth comb =
    let consComb i = arr.(i) :: comb in
    let recurse i = go (i+1) (depth+1) (consComb i) in
      if depth == k
      then map consComb (enum start n)
      else concat_map recurse (enum start n)
  in go 0 1 []

let sum ls = fold_left (fun x y -> x + y) 0 ls

let product ls = fold_left (fun x y -> x * y) 1 ls

exception InternalError of string

let part1 ls =
  let predicate a = 2020 == sum a in
  match find_opt predicate ls with
  | Some v -> v
  | None -> raise (InternalError "get_solution: no solution found")

let part2 numbers combs =
  let nums = IntSet.of_list numbers in
  let f acc comb =
    let n = 2020 - sum comb in
    if IntSet.mem n nums then n::comb else acc
  in
  fold_left f [] combs

let string_of_int_list ls =
  "[" ^ (String.concat "; " (map string_of_int ls)) ^ "]"

let show_solution part sol =
  let ssol = string_of_int_list sol
  and p = product sol in
  Printf.printf "%s: numbers: %s.  Product: %d\n" part ssol p

let main () =
  let datas = read_datas () in
  let combs = combinations 2 datas in
  show_solution "Part1" (part1 combs) ;
  show_solution "Part2" (part2 datas combs)

let () = if !Sys.interactive then () else main ()
