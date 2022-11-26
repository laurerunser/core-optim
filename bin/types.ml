open PPrint

type ty = 
  (* type variable *)
  | TyVar of tyvar  

  (* function type: TyFun(S, T) is S -> T *)
  | TyFun of ty * ty  

  (* polymorphic type: PolymorphicType(X, T) is forAll X. T *)
  | PolymorphicType of tyvar * ty 

  (* tuple: T_0 * ... * T_k *)  
  | TyTuple of ty list            
[@@deriving show]

and tyvar = string  (* type variable *)
[@@deriving show]

let rec pretty_print_type_paren paren t =
  match t with
  | TyVar x -> !^ x
  | _ -> let ty =
    match t with
    | TyFun (s,t) -> print_ty_fun s t
    | PolymorphicType (x,t) -> print_poly_type x t
    | TyTuple ts -> print_ty_tuple ts
    | _ -> assert false
    in
    group @@
    if paren then parens ty else ty

and print_ty_fun s t =
  ((pretty_print_type_paren true s) ^^ blank 1 ^^ !^ "->")
  ^//^
  (pretty_print_type_paren true t)

and print_poly_type x t =
  (!^ "forall" ^^ space ^^ !^ x ^^ char '.')
  ^//^
  (pretty_print_type_paren true t)

and print_ty_tuple ts =
  separate_map (space ^^ star ^^ space) (pretty_print_type_paren true) ts

let pretty_print_type = pretty_print_type_paren false

let to_string t =
  let b = Buffer.create 16 in
  ToBuffer.pretty 0.8 80 b (pretty_print_type t);
  Buffer.contents b
