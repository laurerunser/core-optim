open Terms
open Types
open Atom

module VarMap = Map.Make (Atom)

let type_error term expected actual =
  failwith
    (Printf.sprintf "term : %s\nxpected type : %s, received %s\n"
       (Terms.to_string term) (Types.to_string expected)
       (Types.to_string actual))

let rec synth (ctxt : ty VarMap.t) (t : term) =
  match t with
  | Var v -> (
      try VarMap.find v ctxt
      with _ -> failwith (Printf.sprintf "The variable %s was not in the type map\n" (Atom.pretty_print_atom v)))
  | Fun (v, ty, t) ->
      let ctxt = VarMap.add v ty ctxt in
      synth ctxt t
  | FunApply (t1, t2) ->
      (* find the type of the function *)
      let ty = synth ctxt t1 in
      let _ =
        (* verify that the argument is the right type *)
        match ty with
        | TyFun (ty1, _) -> check ctxt ty1 t2
        | _ -> failwith "Expected a function type\n"
      in
      ty (* return the type of the function *)
  | Let (v, t, body) ->
      (* find the type of the new binding *)
      let ty = synth ctxt t in
      (* add the binding into the map *)
      let ctxt = VarMap.add v ty ctxt in
      (* find and return the type of the body *)
      synth ctxt body
  | TypeAbstraction (ty_var, t) ->
      (* ignore the X not in freevars *)
      (* also leave the context as it was -> no need to add the X *)
      let ty1 = synth ctxt t in
      (* abstract the type we got for `t` with `ty_var` as the bound variabl *)
      abstract ty_var ty1
  | TypeApply (t, ty) ->
      (* get the general type of `t` *)
      let for_all_ty = synth ctxt t in
      fill for_all_ty ty (* replace the bound variable by `ty` *)
  | TypeAnnotation (t, ty) -> check ctxt ty t

and check (ctxt : ty VarMap.t) (ty : ty) (t : term) =
  let ty1 = synth ctxt t in
  if ty1 = ty then ty else type_error t ty1 ty
