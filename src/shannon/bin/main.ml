(* file: main.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This program is a simple implementation of Shannon's n-gram algorithm.
   Usage:
   > cd src
   > dune exec bin/main.exe
   or
   this code can be run in an OCaml REPL.
*)

(* Code for implementing a map relating keys (i.e., strings) to lists of chars.
 * See the API for Map in OCaml's Standard Library.
 *)
module Map = Map.Make(String)

let makeMap () = Map.empty

let mapToString map =
  let charListToString v =
    "[" ^ (String.concat "; " (List.map Char.escaped v)) ^ "]" in
  let kvStr key cl rest = ("\n" ^ key ^ "=" ^ (charListToString cl) ^ rest)
  in
  "{" ^ (Map.fold kvStr map "}")

(* extend : string -> char -> int
 *
 * (extend string char degree) returns answer=string+c ensuring that
 * answer is no longer than degree by possibly dropping the leftmost
 * letter.
 *)
let extend string char degree =
  let charS : string = String.make 1 char in
  let answer = String.concat "" [string; charS]
  in
  match (String.length answer) <= degree with
  | true  -> answer
  | false -> String.sub answer 1 degree

(* addChar : char -> string -> map -> map
 *
 * (addChar chr key map) map[key] = [c1; ...; ck], addChar adds
 * chr to the list returning the new map.
*)
let addChar chr key map =
  match Map.mem key map with
  | true ->
    let chrs = (Map.find key map) @ [chr]
    in
	  Map.add key chrs (Map.remove key map)
  | false -> map

(* sentinal : char  --- the special value used for the end of the input. *)
let sentinal = Char.chr 0

(* buildModel : string -> int -> Map.t *)
let buildModel text degree =
  let rec loop prefix i map =
    match i = (String.length text) with
    | true  -> Map.add prefix [sentinal] map
    | false ->
       let nextLetter : char = text.[i] in
       let newMap =
         match Map.mem prefix map with
         | true  -> addChar nextLetter prefix map
         | false -> Map.add prefix [nextLetter] map
       in
       loop (extend prefix nextLetter degree) (i + 1) newMap in
  let emptyMap = makeMap()
  in
  loop "" 0 emptyMap

(* pickALetter : string -> (char list) map -> char
 *
 * (pickALetter key map), let map[key] = [c1; ...; ck], pickALetter
 * returns a randomly chosen character from the list.
*)
let pickALetter key model =
  let chars = Map.find key model in
  let k = Random.int (List.length chars)
  in
  List.nth chars k

(* sample : Map.t -> int -> string *)
let sample model degree =
  let rec loop prefix letter result =
    match letter = sentinal with
    | true  -> result
    | false ->
      let result = Lib.fmt "%s%c" result letter in
      let newPrefix = extend prefix letter degree in
      let nextLetter = pickALetter newPrefix model
      in
      loop newPrefix nextLetter result in
  let initialPrefix = "" in
  let firstLetter = pickALetter initialPrefix model
  in
  loop initialPrefix firstLetter ""

(* shannonMarkov : string -> int -> string *)
let shannonMarkov text degree =
  let model  = buildModel text degree in
  let output = sample model degree
  in
  Lib.pfmt "%s" output

(*****************************************************)

let lincoln =
  "The world will little note, nor long remember what we say here,
but it can never forget what they did here. It is for us the living,
rather, to be dedicated here to the unfinished work which they who fought
here have thus far so nobly advanced. It is rather for us to be here
dedicated to the great task remaining before us -- that from these
honored dead we take increased devotion to that cause for which they
gave the last full measure of devotion -- that we here highly resolve
that these dead shall not have died in vain -- that this nation, under
God, shall have a new birth of freedom -- and that government of the
people, by the people, for the people, shall not perish from the earth."

let frost =
  "Two roads diverged in a yellow wood,
And sorry I could not travel both
And be one traveler, long I stood
And looked down one as far as I could
To where it bent in the undergrowth"

let giuliani =
  "They were pushed. A few cases, they were assaulted. In all cases,
they were put in a corral so far away. Probably the closest they got
is from here to the back of that room. We could do like a… Did you
all watch My Cousin Vinny? You know the movie? It’s one of my favorite
law movies because he comes from Brooklyn. And when, the nice lady who
said she saw, and then he says to her, “How many fingers do I got up?”
And she says a three. Well, she was too far away to see it was only two.
These people were further away than My Cousin Vinny was from the witness.
They couldn’t see a thing."
