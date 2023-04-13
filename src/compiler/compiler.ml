(* open Llvm *)
(* open Ast.TypedAst *)

(* let context = global_context () *)
(* let the_module = create_module context "Dyri" *)
(* let builder = builder context *)
(* let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10 *)
(* let int_type = i64_type context;; *)
(* let int32_type = i32_type context;; *)
(**)
(* let typ_lltype = function *)
(*   | TInt -> int_type *)
(*   | _ -> raise Not_found *)

(* let rec eval_compile = function *)
(*   | { desc = Const (v); _ } -> *)
(*     begin *)
(*       match v with *)
(*       | VInt i -> *)
(*         const_int int_type (Int64.to_int i) *)
(*       | VInt32 i -> *)
(*         const_int int32_type (Int32.to_int i) *)
(*       | VString s -> *)
(*         const_string context s *)
(*       | _ -> assert false *)
(*     end *)
(*   | { desc = Var s; _ } -> *)
(*     Hashtbl.find named_values s *)
(*   | { desc = Let ((s, t), ds); _ } -> *)
(*     let ds = eval_compile { desc = ds; typ = t } in *)
(*     set_value_name s ds; *)
(*     Hashtbl.add named_values s ds; *)
(*     ds *)
(*   | { desc = Fun ((s, p), ds); typ = t } -> *)
(*       let pm = [| p |] in *)
(*       let ft = function_type (typ_lltype t) (Array.map typ_lltype pm) in *)
(*     let f = declare_function s ft the_module in *)
(*     let bb = append_block context "" f in *)
(*     position_at_end bb builder; *)
(*     Array.map ( *)
(*       fun a -> *)
(*         let n = s in *)
(*         set_value_name n a; *)
(*         Hashtbl.add named_values n a; *)
(*     ) (params f) |> ignore; *)
(*     let _ = build_ret (eval_compile { desc = ds; typ = TGeneric }) builder in *)
(*     f *)
(*   | { desc = Apply (s, p); _ } -> *)
(*     let f = match lookup_function s the_module with *)
(*       | Some v -> v *)
(*       | None -> raise Not_found *)
(*     in *)
(*     let args = Array.of_list p |> Array.map (fun a -> eval_compile {desc = a; typ = TGeneric}) in *)
(*     build_call f args "" builder *)
(*   | _ -> assert false;; *)

let compile _ast =
  (* List.map (fun a -> eval_compile a ) _ast |> ignore; *)
  (* string_of_llmodule the_module *)
  ""
;;
(* Code used to test...
   let ft = function_type int_type ([|int_type|]) in
   let f = declare_function "test" ft the_module in
   Array.map (
    fun a ->
      let n = "x" in
      set_value_name n a;
      Hashtbl.add named_values n a;
   ) (params f) |> ignore;
   let bb = append_block context "" f in
   position_at_end bb builder;
   let adval = build_add (eval_compile (Var "x")) (eval_compile (Const (VInt 10))) "addtmp" builder in
   let mul = build_mul (eval_compile (Const (VInt 10))) adval "multmp" builder in
   let _ = build_ret mul builder in

   let ft = function_type int_type ([||]) in
   let fm = declare_function "main" ft the_module in
   let bb = append_block context "" fm in
   position_at_end bb builder;
   let param = [|(eval_compile (Const (VInt 5)))|] in
   let mm = build_call f param "test" builder in
   let _ = build_ret mm builder in

   ;;
*)
