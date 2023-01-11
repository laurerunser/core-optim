open Terms
open Types
open PPrint

type frame =
  | HoleFun of term (* applied function: f(term) -> where f is not given *)
  | HoleType of
      ty (* instantiated polymorphism: T[ty] -> where T is not given *)

and stack = frame list

let rec plug (s : stack) (t : term) =
  match s with
  | [] -> t
  | f :: s -> (
      let filled_term = plug s t in
      match f with
      | HoleFun arg -> FunApply (filled_term, arg)
      | HoleType arg -> TypeApply (filled_term, arg))

let pretty_print_frame f =
  match f with
  | HoleFun arg ->
      let f = string "_" in
      let x = get_term_with_parens arg in
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
