open Ast

module V = struct
  type t = tvar

  let equal v1 v2 = v1.id = v2.id
  let compare v1 v2 = Stdlib.compare v1.id v2.id

  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = !r; def = None }
end

let rec head = function TVar { def = Some t; _ } -> head t | t -> t

let rec cannon t =
  match head t with TSeq (t1, t2) -> TSeq (cannon t1, cannon t2) | _ -> t

let rec occur v t =
  match head t with
  | TVar w -> V.equal v w
  | TSeq (t1, t2) -> occur v t1 || occur v t2
  | _ -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TInt, TInt | TBool, TBool | TInt32, TInt32 | TString, TString -> ()
  | TCustom s1, TCustom s2 when s1 = s2 -> ()
  | TVar v1, TVar v2 when V.equal v1 v2 -> ()
  | TVar v1, t2 ->
      if occur v1 t2 then failwith "";
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, TVar _ -> unify t2 t1
  | TSeq (t11, t12), TSeq (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | TSeq (t11, _), t2 -> unify t11 t2
  | t1, TSeq (t21, _) -> unify t1 t21
  | t1, t2 ->
      failwith
        (Format.sprintf "Expected %s but got %s" (Debug.string_of_typ t1)
           (Debug.string_of_typ t2))

module Vset = Set.Make (V)

type schema = { vars : Vset.t; typ : typ }

let rec fvars t =
  match head t with
  | TInt | TInt32 | TString | TBool | TCustom _ -> Vset.empty
  | TSeq (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | TVar v -> Vset.singleton v

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (TVar v)) s) s Vset.empty
