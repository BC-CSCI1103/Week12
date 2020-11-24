(* file: main.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   A few examples working with strings.
*)
let alpha = ['A'; 'C'; 'T'; 'G']

(* exactMatch : string -> string -> bool -- s1 s2 of equal length *)
let exactMatch s1 s2 =
  let len = String.length s1 in
  try
    for i = 0 to len - 1 do
      match s1.[i] = s2.[i] with
      | true  -> ()
      | false -> raise Exit
    done ;
    true
  with | Exit -> false

(* exactMatch : string -> string -> bool -- recursive version *)
let exactMatch s1 s2 =
  let len = String.length s1 in
  let rec repeat i =
    match i = len with
    | true  -> true
    | false -> s1.[i] = s2.[i] && (repeat (i + 1))
  in
  repeat 0

(* hammingDistance : string -> string -> int -- s1, s2 of equal length *)
let hammingDistance s1 s2 =
  let count = ref 0 in
  let len = String.length s1
  in
  for i = 0 to len - 1 do
    if s1.[i] = s2.[i] then () else count := !count + 1
  done ;
  !count

let hammingDistance s1 s2 =
  let len = String.length s1 in
  let rec repeat i count =
    match i = len with
    | true  -> count
    | false ->
      (match s1.[i] = s2.[i] with
       | true  -> repeat (i + 1) count
       | false -> repeat (i + 1) (count + 1))
  in
  repeat 0 0

(* matchesHere : int -> string -> string *)
let matchesHere i s1 s2 =
  let s1Len = String.length s1 in
  try
    for j = 0 to s1Len - 1 do
      match s1.[j] = s2.[i + j] with
      | true  -> ()
      | false -> raise Exit
    done ;
    true
  with | Exit -> false

(* substring : string -> string -> bool -- Is s1 a substring of s2? *)
let substring s1 s2 =
  let s2Len = String.length s2 in
  let s1Len = String.length s1 in
  try
    for i = 0 to s2Len - s1Len do
      match matchesHere i s1 s2 with
      | true  -> raise Exit
      | false -> ()
    done ;
    false
  with | Exit -> true

(* stringsOver : char list -> int -> string list *)
let stringsOver chars n =
  failwith ""


let rec drop x xs =
  match xs with
  | [] -> []
  | y :: ys ->
    if x = y then ys else y :: drop x ys

(* permutations : 'a list -> 'a list list

   [3] => [[3]]
   [2; 3] => [ [2; 3]
             ; [3; 2]
             ]
   [1; 2; 3] => [ [1; 2; 3]  1 :: [2; 3]
                ; [1; 3; 2]  1 :: [3; 2]
                ; [2; 1; 3]
                ; [2; 3; 1]
                ; [3; 1; 2]
                ; [3; 2; 1]
                ]
*)

let rec permutations xs =
  match xs with
  | [] -> []
  | [x] -> [[x]]
  | ys ->
    List.fold_left
      (fun acc x ->
         acc @ List.map (fun zs -> x :: zs) (permutations (drop x ys)))
      []
      ys

let rec permutations xs =
  match xs with
  | [] -> []
  | [x] -> [[x]]
  | _ ->
    let folder acc x =
      let tails = permutations (drop x xs)
      in
      acc @ List.map (fun zs -> x :: zs) tails
    in
    List.fold_left folder [] xs

(* On [1; 2; 3] => (folder (folder (folder [] 1) 2) 3) *)
