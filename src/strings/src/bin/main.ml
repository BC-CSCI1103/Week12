(* file: main.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   A few examples working with strings.
*)

let alpha = ['A'; 'C'; 'T'; 'G']

(* exactMatch : string -> string -> bool *)
let exactMatch s1 s2 =
  try
    for i = 0 to String.length s1 - 1 do
      if s1.[i] = s2.[i] then () else raise Exit
    done ;
    true
  with | Exit -> false

(* exactMatch : string -> string -> bool -- recursive version *)
let exactMatch s1 s2 =
  let n = String.length s1 in
  let rec loop i =
    match i = n with
    | true  -> true
    | false -> s1.[i] = s2.[i] && loop (i + 1)
  in
  loop 0

(* hammingDistance : string -> string -> int -- s1, s2 of equal length *)
let hammingDistance s1 s2 =
  let n = String.length s1 in
  let rec loop i count =
    match i = n with
    | true  -> count
    | false ->
      if s1.[i] = s2.[i] then
        loop (i + 1) count
      else
        loop (i + 1) (count + 1)
  in
  loop 0 0

(* matchesHere : int -> string -> string *)
let matchesHere i s1 s2 =
  try
    for j = 0 to String.length s1 - 1 do
      if s1.[j] = s2.[i + j] then () else raise Exit
    done ;
    true
  with | Exit -> false

(* substring : string -> string -> bool -- Is s1 a substring of s2? *)
let substring s1 s2 =
  let s2Len = String.length s2
  in
  try
    for i = 0 to s2Len - (String.length s1) - 1 do
      match matchesHere i s1 s2 with
      | true  -> raise Exit
      | false -> ()
    done ;
    false
  with | Exit -> true

(* stringsOver : char list -> int -> string list *)
let stringsOver chars n =
  let rec repeat n =
    match n = 0 with
    | true  -> [[]]
    | false ->
      let almost = repeat (n - 1)
      in
      List.fold_right (@)
        (List.map (fun xs -> List.map (fun c -> c :: xs) chars) almost) []
  in
  List.map Lib.implode (repeat n)
