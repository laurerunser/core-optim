(* This module represents evaluation contexts.

   The stack is a list of frames. The first element of the stack is the
   innermost one in the evaluation.
   For example: `(((t u1) u2) u3)`
   is represented by the stack `[HoleFun u1, HoleFun u2, HoleFun u3]` *)

module VarSet = Terms.VarSet
module VarMap = Terms.VarMap

type 'a scoped = {
  scope : 'a;
  vars_term : VarSet.t;
  p_term : Terms.base VarMap.t;
  vars_ty : VarSet.t;
  p_ty : Types.ty VarMap.t;
}

type frame =
  | HoleFun of Terms.base
  | HoleType of Types.ty
  | HoleIf of Terms.term * Terms.term

and stack = frame list

val empty_scope : 'a -> 'a scoped
(* [empty_scope t] returns t with a scope where all the maps and sets are empty *)

val inherit_scope : 'a -> 'b scoped -> 'a scoped
(* [empty_scope t old] returns the scope of old with the term t *)

val scope_with_new_var :
  term:Terms.term ->
  scope:Terms.term scoped ->
  var:Atom.t ->
  base:Terms.base ->
  Terms.term scoped

val scope_with_new_ty :
  term:Terms.term ->
  scope:Terms.term scoped ->
  var:Atom.t ->
  ty:Types.ty ->
  Terms.term scoped

val well_scoped_term : Terms.term scoped -> bool
(* [well_scoped_term t] checks if the scoped term t is well constructed *)

val discharge_term : Terms.term scoped -> Terms.term
(* [discharge_term t] applies the substitutions t.p_term and t.p_ty to t.scope.
   [t] must be a well_scoped term *)

val discharge_base : Terms.base scoped -> Terms.base
(* [discharge_base b] applies the substitution t.p_term to t.scope*)

val discharge_ty : Types.ty scoped -> Types.ty
(* [discharge_ty t] applies the substitution t_py to t.scope *)

val plug : stack -> Terms.term -> Terms.term
(* [plug s t] plugs the first hole in the stack s with the term t
   and propagates the results to the rest of the stack.
   It returns the new term.
   For example, with `s = [HoleFun u1, HoleFun u2]`, it returns the term `((t u1) u2)` *)

val pp_frame : Format.formatter -> frame -> unit
val pp_stack : Format.formatter -> stack -> unit
val pretty_print_frame : frame -> PPrint.document
val pretty_print : frame list -> PPrint.document
val to_string : frame list -> string
(* pretty printing functions *)

val synth_stack : stack -> Types.ty -> Types.ty VarMap.t -> Types.ty
val simplify : Terms.term -> Terms.term
(* [simplify t] simplifies the term [t]
   The simplifications:
   - beta-reduction :
        - `(fun x.t) a` becomes `t[x\a]`
        - `(fun [X].t) Y` becomes `t[X\Y]`
    - if branches simplification :
        - `if true then a else b` becomes `a`
        - `if false then a else b` becomes `b`
*)
