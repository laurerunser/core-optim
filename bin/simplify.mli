val simplify : Terms.term -> Terms.term
(* [simplify t acc p_var p_ty] simplifies the term [t] with the evaluation context [acc].
   It also appplies the substitutions [p_var] on variables and [p_ty] on type variables.
   The simplifications:
   - beta-reduction :
        - `(fun x.t) a` becomes `t[x\a]`
        - `(fun [X].t) Y` becomes `t[X\Y]`
    - if branches simplification :
        - `if true then a else b` becomes `a`
        - `if false then a else b` becomes `b`
*)
