open Terms
open Types
open Stack

exception Type_Error of ty

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

let type_if_branches_error branch expected actual =
  type_error
    (Printf.sprintf
       "Branches do not have the same type\n\
        Branch: %s\n\
        Received type: %s\n\
        Expected %s" (Terms.to_string branch) (Types.to_string actual)
       (Types.to_string expected))

let type_ite_frame_error frame actual =
  type_error
    (Printf.sprintf "Frame: %s\nExpected a bool type\nReceived type: %s"
       (Stack.to_string [ frame ])
       (Types.to_string actual))

let rec synth (ctxt : ty VarMap.t) (t : term) =
  match t with
  | Base (Bool _) -> TyBool
  | Base (Var v) -> (
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
      | TyFun (ty1, ty2) -> (
          try
            let _ = check ctxt ty1 (Base t2) in
            ty2
          with Type_Error ty -> type_check_error (Base t2) ty ty1)
      | _ -> type_synth_error t ty "function")
  | Let (v, t, body) ->
      (* find the type of the new binding *)
      let ty = synth ctxt t in
      (* add the binding into the map *)
      let ctxt = VarMap.add v ty ctxt in
      (* find and return the type of the body *)
      synth ctxt body
  | IfThenElse (e1, e2, e3) -> (
      let _ =
        (* check that the condition is a boolean (either true or false) *)
        try check ctxt TyBool e1
        with Type_Error actual_ty -> type_check_error e1 TyBool actual_ty
      in
      (* check that both branches have the same type *)
      let ty = synth ctxt e2 in
      try check ctxt ty e3
      with Type_Error actual_ty -> type_if_branches_error e3 ty actual_ty)
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
  | TypeAnnotation (t, ty) -> (
      try check ctxt ty t with Type_Error ty' -> type_check_error t ty' ty)

and check (ctxt : ty VarMap.t) (ty : ty) (t : term) =
  let ty1 = synth ctxt t in
  if Types.equal_ty ty1 ty then ty else raise (Type_Error ty1)

let rec synth_stack (s : stack) (ty : ty) (ctxt : ty VarMap.t) =
  match s with
  | [] -> ty
  | f :: s -> (
      match f with
      | HoleFun arg -> (
          match ty with
          | TyFun (a, b) -> (
              try
                let _ = check ctxt a (Base arg.scope) in
                synth_stack s b ctxt
              with Type_Error ty' -> type_fun_frame_error f ty ty')
          | _ -> type_fun_frame_error f ty (synth ctxt (Base arg.scope)))
      | HoleType arg -> (
          try synth_stack s (fill ty arg.scope) ctxt
          with Not_Polymorphic -> type_poly_frame_error f ty)
      | HoleIf (e1, e2) ->
          if ty <> TyBool then type_ite_frame_error f ty
          else
            let ty1 = synth ctxt e1.scope in
            let ty2 = synth ctxt e2.scope in
            if Types.equal_ty ty1 ty2 then ty1
            else type_if_branches_error e2.scope ty1 ty2)
