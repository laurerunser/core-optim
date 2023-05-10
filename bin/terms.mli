type base = Var of Atom.t | Bool of bool

type term =
  | Base of base
  | Fun of Atom.t * Types.ty * term
  | FunApply of term * base
  | Let of Atom.t * term * term
  | IfThenElse of term * term * term
  | TypeAbstraction of Atom.t * term
  | TypeApply of term * Types.ty
  | TypeAnnotation of term * Types.ty

val pretty_print : term -> PPrint.document
val get_term_with_parens : term -> PPrint.document
val print_abstraction : Atom.t -> Types.ty -> term -> PPrint.document
val print_fun_apply : term -> base -> PPrint.document
val print_let_in : Atom.t -> term -> term -> PPrint.document
val print_type_abstraction : Atom.t -> term -> PPrint.document
val print_type_apply : term -> Types.ty -> PPrint.document
val print_type_annotation : term -> Types.ty -> PPrint.document
val print_base : base -> PPrint.document
val to_string : term -> string

module VarSet = Types.VarSet

val free_vars_base : base -> VarSet.t

val free_vars : term -> VarSet.t
(** [free_vars t] returns the free variables of t *)

val free_ty_vars_of_term : term -> VarSet.t
val pp_term : Format.formatter -> term -> unit
val pp_base : Format.formatter -> base -> unit
val equal_term : term -> term -> bool

module VarMap = Types.VarMap

val sub_var : Atom.t -> base VarMap.t -> base
val alpha_eq : term -> term -> bool

exception Type_Error of Types.ty

val type_error : string -> 'a
val type_if_branches_error : term -> Types.ty -> Types.ty -> 'a
val synth : Types.ty VarMap.t -> term -> Types.ty
val check : Types.ty VarMap.t -> Types.ty -> term -> Types.ty