type term =
  | Atom of full_atom
  | Fun of variable * Types.ty * term
  | FunApply of term * full_atom
  | Let of variable * term * term
  | IfThenElse of term * term * term
  | TypeAbstraction of Types.tyvar * term
  | TypeApply of term * Types.ty
  | TypeAnnotation of term * Types.ty

and variable = Atom.t
and full_atom = Var of variable | Bool of bool

val pretty_print : term -> PPrint.document
val get_term_with_parens : term -> PPrint.document
val print_abstraction : variable -> Types.ty -> term -> PPrint.document
val print_fun_apply : term -> full_atom -> PPrint.document
val print_let_in : variable -> term -> term -> PPrint.document
val print_type_abstraction : Types.tyvar -> term -> PPrint.document
val print_type_apply : term -> Types.ty -> PPrint.document
val print_type_annotation : term -> Types.ty -> PPrint.document
val print_full_atom : full_atom -> PPrint.document
val to_string : term -> string

module VarSet : sig
  type elt = variable
  type t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

val free_vars : term -> VarSet.t
(** [free_vars t] returns the free variables of t *)

val pp_term : Format.formatter -> term -> unit
val pp_variable : Format.formatter -> variable -> unit
val pp_full_atom : Format.formatter -> full_atom -> unit
val equal_term : term -> term -> bool
val equal_variable : variable -> variable -> bool