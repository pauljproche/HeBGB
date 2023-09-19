exception Unimplemented of string
exception Bug of string

module L = Llvm
open Sast
module StringMap = Map.Make(String)
type rho = L.llvalue StringMap.t

(* Important notes:
 *
 * In HeBGB _every_ value is heap allocated. That means that the only
 * values passed around in registers should be pointers to these heap
 * allocated values. Real values (such as i32s) should only be stored
 * in temporaries for a short duration for the purposes of application
 * to llvm opcodes such as add, sub, load, store, etc.
 *
 * Calls to infix_expr and expr should _never_ return an llvalue that
 * points to a non i8* type. To ensure that each case within expr
 * remains compatible with all the others, we _only_ return i8*s
 * from expr. i8* (ptr_t) represents a kind of "void" pointer which
 * will be cast to the correct llvm type at the site of its usage
 * (which is guaranteed safe due to semantic analysis).
 *
 *)

let translate (se : sexpr) =
  let context = L.global_context () in
  let i32_t   = L.i32_type  context in
  let i8_t    = L.i8_type   context in
  let i1_t    = L.i1_type   context in
  let ptr_t   = L.pointer_type (L.i8_type context) in

  let the_module = L.create_module context "HeBGB" in

  let print_int_t : L.lltype =
    L.function_type i32_t [| i32_t |] in
  let print_int_func : L.llvalue =
    L.declare_function "print_int" print_int_t the_module in

  let print_str_t : L.lltype =
    L.function_type i32_t [| L.pointer_type i8_t |] in
  let _print_str_func : L.llvalue =
    L.declare_function "print_string" print_str_t the_module in

  let add_terminal builder instr =
    (* The current block where we're inserting instr *)
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in

  let rec expr (outer_fun : L.llvalue) (builder : L.llbuilder) (locals : rho) (se : sexpr) =
    let rec infix_expr (builder : L.llbuilder) (a : sexpr) (op : Ast.op) (b : sexpr) =
      (* first, we get the raw pointers corresponding to a and b *)
      let builder, addr_of_a = ex builder a in
      let builder, addr_of_b = ex builder b in
      (* then we cast the raw pointers to int pointers (since we know
       * we have ints at this point due to semantic analysis *)
      let i32_ptr_of_a = L.build_pointercast addr_of_a (L.pointer_type i32_t)
                           "tmp" builder in
      let i32_ptr_of_b = L.build_pointercast addr_of_b (L.pointer_type i32_t)
                           "tmp" builder in
      (* now, we load the actual int values into registers *)
      let value_of_a = L.build_load i32_ptr_of_a "infix_operand" builder in
      let value_of_b = L.build_load i32_ptr_of_b "infix_operand" builder in
      let f =
        match op with
        | Ast.Add     -> L.build_add
        | Ast.Sub     -> L.build_sub
        | Ast.Mul     -> L.build_mul
        | Ast.Div     -> L.build_sdiv
        | Ast.Mod     -> L.build_srem
        | Ast.Less    -> L.build_icmp L.Icmp.Slt
        | Ast.Greater -> L.build_icmp L.Icmp.Sgt
        | Ast.Leq     -> L.build_icmp L.Icmp.Sle
        | Ast.Geq     -> L.build_icmp L.Icmp.Sge
        | Ast.Eq      -> L.build_icmp L.Icmp.Eq
        | Ast.Neq     -> L.build_icmp L.Icmp.Ne
        | Ast.Or      -> L.build_or
        | Ast.And     -> L.build_and in
      let value_of_result = f value_of_a value_of_b "tmp" builder in
      let int_value_of_result = L.build_intcast value_of_result i32_t "tmp" builder in
      (* allocate a new cell on the heap and put the result inside *)
      let cell = L.build_malloc i32_t "tmp" builder in
      ignore (L.build_store int_value_of_result cell builder);
      (* cast back to raw pointers as is tradition *)
      L.build_pointercast cell ptr_t "cell" builder

    and ex (builder : L.llbuilder) ((_, exp) : sexpr) =
      match exp with
      | SId nm when Option.is_some (int_of_string_opt nm) ->
         (* in this case, Id points to an index inside the
          * current closure *)
         let idx = int_of_string nm in
         let clo_ptr = match StringMap.find_opt "curr_clo" locals with
           | Some ptr -> ptr
           | None -> raise (Bug "could not find curr_clo in locals") in
         (* NOTE: the location of the pointer to the bound value "nm"
          *       is the index plus 1, since the first index is used
          *       for the function pointer associated with this lambda *)
         let arr_typ = L.pointer_type (L.array_type ptr_t (idx + 2)) in
         let clo_arr_ptr = L.build_pointercast clo_ptr arr_typ "clo_arr_ptr" builder in
         let id_loc = L.build_in_bounds_gep clo_arr_ptr [| L.const_int i32_t 0;
                                                           L.const_int i32_t (idx + 1) |]
                        "id_loc" builder in
         builder, L.build_load id_loc "id_ptr" builder
      | SId nm -> (match StringMap.find_opt nm locals with
                   | Some ptr -> builder, ptr
                   | None -> raise (Bug ("unbound name in codegen (" ^ nm ^ ")")))
      | SLitInt i ->
         (* allocate a new cell *)
         let cell = L.build_malloc i32_t "tmp" builder in
         (* store the integer value in the newly created cell *)
         ignore (L.build_store (L.const_int i32_t i) cell builder);
         (* we return the address of the new integer *)
         builder, L.build_pointercast cell ptr_t "cell" builder
      | SLitStr _s -> raise (Unimplemented "codegen::SLitStr")
      | SLitList _es -> raise (Unimplemented "codegen::SLitList")
      | SLitTup _es -> raise (Unimplemented "codegen::SLitTup")
      | SInfix (a, op, b) -> builder, infix_expr builder a op b
      | SBind (_, nm, e, rest) ->
         let builder, ptr_to_value = ex builder e in
         let locals' = StringMap.add nm ptr_to_value locals in
         expr outer_fun builder locals' rest
      | SAssign (_nm, _e, _rest) -> raise (Unimplemented "codegen::SAssign")
      | SFun finfo ->
         let fdef = match finfo.llvm_decl with
           | Some def -> def
           | None -> raise (Bug "encountered un-elevated lambda in codegen") in
         (* cast the function pointer to uint8* type *)
         let f_ptr = L.build_bitcast fdef ptr_t "f_ptr" builder in
         (* allocate a closure *)
         let clo_type = L.array_type ptr_t (Array.length finfo.free_variables + 1) in
         let clo = L.build_malloc clo_type "clo" builder in
         let f_ptr_loc = L.build_in_bounds_gep clo [| L.const_int i32_t 0;
                                                      L.const_int i32_t 0 |]
                           "f_ptr_loc" builder in
         ignore (L.build_store f_ptr f_ptr_loc builder);
         let clo_ptr = L.build_bitcast clo ptr_t "clo_ptr" builder in
         (* store pointers to each captured variable inside the closure *)
         Array.iteri (fun idx var ->
             let var_ptr_loc = L.build_in_bounds_gep clo [| L.const_int i32_t 0;
                                                            L.const_int i32_t (idx + 1) |]
                                 "var_loc" builder in
             let ignored_type = Ast.TyInt in (* could by any type; it will be ignored *)
             let builder, var_ptr =
               if finfo.name = var then
                 (* if the variable is the name of the function itself,
                  * we return the closure pointer. *)
                 builder, clo_ptr
               else
                 expr outer_fun builder locals (ignored_type, SId var) in
             ignore (L.build_store var_ptr var_ptr_loc builder);
           ) finfo.free_variables;

         let locals' = StringMap.add finfo.name clo_ptr locals in
         expr outer_fun builder locals' finfo.rest
      | SApp ((_, SId "p"), args) ->
         let arg_sexpr = List.hd args in
         let arg_ty, _ = arg_sexpr in
         let builder, arg_ptr = ex builder arg_sexpr in
         ignore (match arg_ty with
                 | TyInt ->
                    let arg_i32_ptr = L.build_pointercast arg_ptr (L.pointer_type i32_t)
                                        "tmp" builder in
                    let arg_value = L.build_load arg_i32_ptr "tmp" builder in
                    L.build_call print_int_func [| arg_value |]
                      "print_int" builder
                 | _ -> raise (Unimplemented "codegen::SApp(p)"));
         (* always return 0 from print functions *)
         ex builder (TyInt, SLitInt 0)
      | SApp (f, args) ->
         (* evaluate all of the argument expressions *)
         let arity = List.length args in
         let ftype = L.function_type ptr_t (Array.init (arity + 1) (fun _ -> ptr_t)) in
         let builder, arg_val_ptrs =
           List.fold_left (fun (builder, arg_ptrs) arg_sexpr ->
               let builder, arg_ptr = ex builder arg_sexpr in
               builder, arg_ptr::arg_ptrs) (builder, []) args in
         let builder, val_ptr = ex builder f in
         (* val_ptr is a pointer to the closure. The first element of the
            closure is just a function pointer, so we de-reference to
            pointer to pointer to function *)
         let clo_val_ptr = L.build_pointercast val_ptr
                             (L.pointer_type (L.pointer_type ftype))
                             "clo_ptr" builder in
         (* then, we perform a load, so clo_val has type pointer to function *)
         let f = L.build_load clo_val_ptr "clo_val" builder in
         builder,
         L.build_call f (Array.of_list (arg_val_ptrs @ [val_ptr])) "f_result" builder
      | SCont (_, _nm, _alpha, _body, _rest) -> raise (Unimplemented "codegen::SCont")
      | SIf (test_e, do_e, else_e) ->
         let builder, test = ex builder test_e in
         let result_loc = L.build_alloca ptr_t "result" builder in

         let merge_bb = L.append_block context "merge" outer_fun in
         (* do branch *)
         let do_bb = L.append_block context "do" outer_fun in
         let do_builder = L.builder_at_end context do_bb in
         let do_builder, do_val_ptr = expr outer_fun do_builder locals do_e in
         ignore (L.build_store do_val_ptr result_loc do_builder);
         add_terminal do_builder (L.build_br merge_bb);
         (* else branch *)
         let else_bb = L.append_block context "else" outer_fun in
         let else_builder = L.builder_at_end context else_bb in
         let else_builder, else_val_ptr = expr outer_fun else_builder locals else_e in
         ignore (L.build_store else_val_ptr result_loc else_builder);
         add_terminal else_builder (L.build_br merge_bb);
         (* extract the integer value of test and cast it to i1_t *)
         let i32_ptr_of_test =
           L.build_pointercast test (L.pointer_type i32_t)
             "tmp" builder in
         let test_val = L.build_load i32_ptr_of_test "test_val" builder in
         let test_bool_val = L.build_intcast test_val i1_t "test_bool_val" builder in
         ignore (L.build_cond_br test_bool_val do_bb else_bb builder);
         let merge_builder = L.builder_at_end context merge_bb in
         let result = L.build_load result_loc "result" merge_builder in
         merge_builder, result in
    ex builder se in

  let elevate_lambdas (s : sexpr) =
    let rec f = function
      | typ, SFun finfo ->
         (* NOTE: Every function in HeBGB gets an extra hidden argument
          *       called curr_clo which contains a pointer to the closure
          *       allocated for that function. On function application,
          *       the closure that was applied is passed back into this
          *       last argument.
          *
          *       All captured free variables are renamed in the semantic
          *       analysis stage to positive integers. These positive integers
          *       are used as an index that the code generation stage can use
          *       to the find the value of a captured variable in curr_clo. *)
         let arity = List.length finfo.parameters in
         let ftype = L.function_type ptr_t (Array.init (arity + 1) (fun _ -> ptr_t)) in
         let fdef = L.define_function finfo.name ftype the_module in
         let builder = L.builder_at_end context (L.entry_block fdef) in

         let add_formal m n p =
           let () = L.set_value_name n p in
	   StringMap.add n p m in

         let parameter_names = (List.map snd finfo.parameters) @ ["curr_clo"] in
         let locals = List.fold_left2 add_formal StringMap.empty parameter_names
                        (Array.to_list (L.params fdef)) in
         (* for every variable that we captured in the body of this function,
          * we add a binding to locals that references its actual name. This
          * allows functions that close over the body of _this_ function to
          * lookup the variable by name when they are constructing their closure *)
         let _, locals' =
           Array.fold_left (fun (idx, locals) var ->
               let ignored_type = Ast.TyInt in (* could by any type; it will be ignored *)
               let idx_name = Int.to_string idx in
               (* in this case, we ignore the returned builder because
                * we know that an If expression cannot be generated here *)
               let _, v = expr fdef builder locals (ignored_type,
                                                    SId idx_name) in
               idx + 1, StringMap.add var v locals
             ) (0, locals) finfo.free_variables in
         let body' = f finfo.body in
         let builder, ret_reg = expr fdef builder locals' body' in
         ignore (L.build_ret ret_reg builder);
         typ, SFun { finfo with llvm_decl = Some fdef;
                                body = body';
                                rest = f finfo.rest }
      | typ, SLitList es -> typ, SLitList (List.map f es)
      | typ, SLitTup es -> typ, SLitTup (List.map f es)
      | typ, SInfix (a, op, b) -> typ, SInfix (f a, op, f b)
      | typ, SBind (t, nm, v, rest) -> typ, SBind (t, nm, f v, f rest)
      | typ, SAssign (nm, v, rest) -> typ, SAssign (nm, f v, f rest)
      | typ, SApp (fe, es) -> typ, SApp (f fe, List.map f es)
      | typ, SCont (t, nm, alpha, v, rest) -> typ, SCont (t, nm, alpha, f v, f rest)
      | typ, SIf (a, b, c) -> typ, SIf (f a, f b, f c)
      | typ, v -> typ, v in (* all sexprs with no children *)
    f s in

  let main_fn_type = L.function_type i32_t [| |] in
  let main_fn = L.define_function "main" main_fn_type the_module in
  let builder = L.builder_at_end context (L.entry_block main_fn) in
  let se' = elevate_lambdas se in
  let builder, ret_reg = expr main_fn builder StringMap.empty se' in
  let i32_ret_ptr = L.build_bitcast ret_reg (L.pointer_type i32_t) "tmp" builder in
  let i32_ret_value = L.build_load i32_ret_ptr "ret_code" builder in
  ignore (L.build_ret i32_ret_value builder);
  the_module
