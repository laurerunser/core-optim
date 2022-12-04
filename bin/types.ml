open PPrint

type ty = 
  (* type variable *)
  | TyFreeVar of tyvar  
  | TyBoundVar of int

  (* function type: TyFun(S, T) is S -> T *)
  | TyFun of ty * ty  

  (* polymorphic type: PolymorphicType(X, T) is forAll X. T *)
  | PolymorphicType of tyvar * ty 

  (* tuple: T_0 * ... * T_k *)  
  | TyTuple of ty list            
[@@deriving show]

and tyvar = string  (* type variable *)
[@@deriving show]


(********** Type manipulation **********)

(* [replace_ty x ty c] replaces the free variable
   [x] with the bound variable [c] in the type [ty]. 
   It goes down the type recursively and increments [c]
   each time it enters a new PolyMorphicType binding. *)
let rec replace_ty x ty c = 
  match ty with
  | PolymorphicType(_, t) -> replace_ty x t (c+1)
  | TyFreeVar(v) when v = x -> TyBoundVar(c)
  | TyFun(t1, t2) -> 
    let t1 = replace_ty x t1 c in 
    let t2 = replace_ty x t2 c in 
    TyFun(t1, t2)
  | TyTuple(l) -> 
    let l' = List.map (fun t -> replace_ty x t c) l in 
    TyTuple(l')
  | _ as t -> t

(* [abstract X ty] returns a new type `forall X. ty` where
   the free variable [X] is replaced with a bound variable. *)
let abstract x ty = 
  let ty = replace_ty x ty 0
  in PolymorphicType(x, ty)
  

(********** Pretty printing **********)
let rec pretty_print_type_paren paren t =
  match t with
  | TyFreeVar x -> !^ x
  | TyBoundVar x -> !^ (Int.to_string x)
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
