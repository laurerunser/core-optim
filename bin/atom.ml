
type atom = {
  indentifier : string [@equal fun _ _ -> true] [@compare fun _ _ -> 0];
  number : int
}
[@@deriving show, eq, ord]

let pretty_print_atom a = Format.asprintf "%s__%d" a.indentifier a.number

let number = 
  let c = ref 0 in
  fun () -> incr c; !c

let fresh ident = {
  indentifier = ident;
  number = number()
}
