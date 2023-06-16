open Ast.Typed

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

let rec head = function Tvar { def = Some t; _ } -> head t | t -> t

let canon t1 =
  match head t1 with Tarrow (t1, t2) -> Tarrow (head t1, head t2) | t -> t

let rec occur tv t1 =
  match head t1 with
  | Tvar w -> V.equal tv w
  | Tarrow (t1, t2) -> occur tv t1 || occur tv t2
  | _ -> false

let rec unify ~pos t1 t2 : unit =
  match (head t1, head t2) with
  | Tvar t1, Tvar t2 when V.equal t1 t2 -> ()
  | Tvar t1, t2 ->
      if occur t1 t2 then Errors.type_error (canon (Tvar t1)) (canon t2) pos;
      (* unification_error (Tvar t1) t2; *)
      assert (t1.def = None);
      t1.def <- Some t2
  | t1, Tvar _ -> unify ~pos t2 t1
  | Tarrow (t11, t12), Tarrow (t21, t22) ->
      unify ~pos t11 t21;
      unify ~pos t12 t22
  | Tarrow (t1, _), t2 -> unify ~pos t1 t2
  | t1, Tarrow (t2, _) -> unify ~pos t1 t2
  | t1, t2 when t1 = t2 -> ()
  | t1, t2 -> Errors.type_error (canon t1) (canon t2) pos

module VSet = Set.Make (V)
module VMap = Map.Make (V)
module HashMap = Map.Make (String)

let bindings_empty = HashMap.empty
let bindings_add = HashMap.add
let bindings_find = HashMap.find
let vset_empty = HashMap.empty
let vset_add = HashMap.add
let vset_find = HashMap.find

type schema = { vars : VSet.t; typ : typ }

let rec fvars t =
  match head t with
  | Tvar ({ def = None; _ } as t1) -> VSet.singleton t1
  | Tarrow (t1, t2) -> VSet.union (fvars t1) (fvars t2)
  | Tproduct (t1, t2) -> VSet.union (fvars t1) (fvars t2)
  | _ -> VSet.empty

(*
   bindings - Variables inside the real context GAMA
   fvars - free variables *)
type t = { bindings : schema HashMap.t; fvars : VSet.t }

let empty = { bindings = bindings_empty; fvars = VSet.empty }

let add name (t : typ) env =
  let vt = fvars t in
  let s, fvars = ({ typ = t; vars = VSet.empty }, VSet.union env.fvars vt) in
  { bindings = bindings_add name s env.bindings; fvars }

let add_gen name (t : typ) env =
  let vt = fvars t in
  let s, fvars =
    let env_fvars =
      VSet.fold (fun v s -> VSet.union (fvars (Tvar v)) s) env.fvars VSet.empty
    in
    ({ typ = t; vars = VSet.diff vt env_fvars }, env.fvars)
  in
  { bindings = bindings_add name s env.bindings; fvars }

let find name env =
  let tx = bindings_find name env.bindings in
  let s =
    VSet.fold (fun v s -> VMap.add v (Tvar (V.create ())) s) tx.vars VMap.empty
  in
  let rec subst t =
    match head t with
    | Tvar x as t -> ( try VMap.find x s with Not_found -> t)
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
    | Tproduct (t1, t2) -> Tproduct (subst t1, subst t2)
    | t -> t
  in
  subst tx.typ
