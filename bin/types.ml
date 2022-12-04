open PPrint
exception Not_Polymorphic

type ty = 
  (* type variable *)
  | TyFreeVar of tyvar  
  | TyBoundVar of int

  (* function type: TyFun(S, T) is S -> T *)
  | TyFun of ty * ty  

  (* polymorphic type: PolymorphicType(X, T) is forAll X. T *)
  | PolymorphicType of (tyvar [@opaque][@equal fun _ _ -> true]) * ty

  (* tuple: T_0 * ... * T_k *)  
  | TyTuple of ty list            
[@@deriving show, eq]

and tyvar = string  (* type variable *)
[@@deriving show, eq]


(********** Type manipulation **********)

(* [replace_free x ty c] replaces the free variable
   [x] with the bound variable [c] in the type [ty]. 
   It goes down the type recursively and increments [c]
   each time it enters a new PolyMorphicType binding. *)
let rec replace_free x ty c = 
  match ty with
  | PolymorphicType(y, t) -> 
    PolymorphicType(y, replace_free x t (c+1))
  | TyFreeVar(v) when v = x -> TyBoundVar(c)
  | TyFun(t1, t2) -> 
    let t1 = replace_free x t1 c in 
    let t2 = replace_free x t2 c in 
    TyFun(t1, t2)
  | TyTuple(l) -> 
    let l' = List.map (fun t -> replace_free x t c) l in 
    TyTuple(l')
  | _ as t -> t

(* [abstract X ty] returns a new type `forall X. ty` where
   the free variable [X] is replaced with a bound variable. *)
let abstract x ty = 
  let ty = replace_free x ty 0
  in PolymorphicType(x, ty)

(* [replace_bound c s t] replaces the bound variable with
   value [c] with the type [s] in the type [t] and decrements
   the others bound variables.
   It does down the type recursively and increments [c]
   each time it enters a new PolymorphicType *)
let rec replace_bound c s = function
  | PolymorphicType(x, t) ->
    PolymorphicType(x, replace_bound (c+1) s t)
  | TyBoundVar x when x = c -> s
  | TyFun(t1, t2) ->
    let t1 = replace_bound c s t1 in
    let t2 = replace_bound c s t2 in
    TyFun(t1, t2)
  | TyTuple l ->
    let l = List.map (replace_bound c s) l in
    TyTuple l
  | t -> t

(* [fill t s] raises Not_Polymorphic if [t] is not a
   PolymorphicType else replaces the bound variable
    of [t] with [s] *)
let fill t s =
  match t with
  | PolymorphicType (_, t) -> replace_bound 0 s t
  | _ -> raise Not_Polymorphic

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
