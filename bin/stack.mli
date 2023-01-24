(* This module represents evaluation contexts.

   The stack is a list of frames. The first element of the stack is the
   innermost one in the evaluation.
   For example: `(((t u1) u2) u3)`
   is represented by the stack `[HoleFun u1, HoleFun u2, HoleFun u3]` *)

type frame = HoleFun of Terms.full_atom | HoleType of Types.ty
and stack = frame list

val pp_frame : Format.formatter -> frame -> unit
val pp_stack : Format.formatter -> stack -> unit
val to_string : frame list -> string
(* pretty printing functions *)

val plug : stack -> Terms.term -> Terms.term
(* [plug s t] plugs the first hole in the stack s with the term t
   and propagates the results to the rest of the stack.
   It returns the new term.
   For example, with `s = [HoleFun u1, HoleFun u2]`, it returns the term `((t u1) u2)` *)
