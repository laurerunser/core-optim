open Terms
open Types
open Stack
module VarMap = Map.Make (Atom)

let type_error msg = failwith (Printf.sprintf "Type error!\n%s" msg)

let type_check_error term expected actual =
  type_error
    (Printf.sprintf "Term : %s\nExpected type: %s\nReceived type: %s\n"
       (Terms.to_string term) (Types.to_string expected)
       (Types.to_string actual))

let type_synth_error term actual expected =
  type_error
    (Printf.sprintf "Term : %s\nExpected a %s type\nReceived type: %s\n"
       (Terms.to_string term) expected (Types.to_string actual))

let type_fun_frame_error frame actual expected =
  type_error
    (Printf.sprintf "Frame: %s\nExpected a %s->_ type\nReceived type: %s\n"
       (Stack.to_string [ frame ])
       (Types.to_string expected) (Types.to_string actual))

let type_poly_frame_error frame actual =
  type_error
    (Printf.sprintf "Frame: %s\nExpected a polymorphic type\nReceived type: %s"
       (Stack.to_string [ frame ])
       (Types.to_string actual))

let rec synth (ctxt : ty VarMap.t) (t : term) =
  match t with
  | Var v -> (
      try VarMap.find v ctxt
      with _ ->
        failwith
          (Printf.sprintf "The variable %s was not in the type map\n"
             (Atom.pretty_print_atom v)))
  | Fun (v, ty, t) ->
      let ctxt = VarMap.add v ty ctxt in
      let ty2 = synth ctxt t in
      TyFun (ty, ty2)
  | FunApply (t1, t2) -> (
      (* find the type of the function *)
      let ty = synth ctxt t1 in
      (* verify that the argument is the right type *)
      match ty with
      | TyFun (ty1, ty2) ->
          let _ = check ctxt ty1 t2 in
          ty2
      | _ -> type_synth_error t ty "function")
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
      let ty = synth ctxt t in
      (* abstract the type we got for `t` with `ty_var` as the bound variabl *)
      abstract ty_var ty
  | TypeApply (t, ty) -> (
      (* get the general type of `t` *)
      let for_all_ty = synth ctxt t in
      try fill for_all_ty ty (* replace the bound variable by `ty` *)
      with Not_Polymorphic -> type_synth_error t ty "polymorphic")
  | TypeAnnotation (t, ty) -> check ctxt ty t

and check (ctxt : ty VarMap.t) (ty : ty) (t : term) =
  let ty1 = synth ctxt t in
  if Types.equal_ty ty1 ty then ty else type_check_error t ty1 ty

let synth_frame (f : frame) (t : term) (ctxt : ty VarMap.t) =
  match f with
  | HoleFun arg -> (
      try synth ctxt (FunApply (t, arg))
      with Failure _ -> type_fun_frame_error f (synth ctxt t) (synth ctxt arg))
  | HoleType arg -> (
      try synth ctxt (TypeApply (t, arg))
      with Failure _ -> type_poly_frame_error f (synth ctxt t))

let rec synth_stack (s : stack) (t : term) (ctxt : ty VarMap.t) =
  match s with
  | [] -> synth ctxt t
  | f :: s -> (
      let ty = synth_stack s t ctxt in
      match f with
      | HoleFun arg -> (
          (* get the type of the argument inside the frame *)
          let arg_ty = synth ctxt arg in
          (* the only way there are compatible is if the frame is a TyFun a->b, and the
             argument is also of type a *)
          match ty with
          | TyFun (a, b) -> (
              try
                let _ = check ctxt a arg in
                b
              with Failure _ -> type_fun_frame_error f ty arg_ty)
          | _ -> type_fun_frame_error f ty arg_ty)
      | HoleType arg -> (
          (* we only need `arg` to have a PolymorphicType *)
          try fill ty arg with Not_Polymorphic -> type_poly_frame_error f ty))
