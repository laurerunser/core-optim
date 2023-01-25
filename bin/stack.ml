open Terms
open Types
open PPrint

type frame =
  | HoleFun of full_atom (* applied function: f(term) -> where f is not given *)
  | HoleType of
      ty (* instantiated polymorphism: T[ty] -> where T is not given *)
[@@deriving show]

and stack = frame list [@@deriving show]

let rec plug (s : stack) (t : term) =
  match s with
  | [] -> t
  | f :: s ->
      let filled_term =
        match f with
        | HoleFun arg -> FunApply (t, arg)
        | HoleType arg -> TypeApply (t, arg)
      in
      plug s filled_term

let pretty_print_frame f =
  match f with
  | HoleFun arg ->
      let f = string "_" in
      let x = print_full_atom arg in
      group @@ prefix 2 1 f x
  | HoleType arg ->
      let t = string "_" in
      group @@ t ^^ lbracket ^^ pretty_print_type arg ^^ rbracket

let rec pretty_print s =
  match s with
  | [] -> empty
  | f :: tail -> pretty_print_frame f ^^ hardline ^^ nest 1 (pretty_print tail)

let to_string s =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print s);
  Buffer.contents b
