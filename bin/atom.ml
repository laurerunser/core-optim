type t = {
  identifier : string; [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
  number : int;
}
[@@deriving show, eq, ord]

let pretty_print_atom a = Format.asprintf "%s__%d" a.identifier a.number

let number =
  let c = ref 0 in
  fun () ->
    incr c;
    !c

let fresh ident = { identifier = ident; number = number () }
