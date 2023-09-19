open Ast
open Sast

exception Unimplemented of string
exception TypeError of expr * string

module StringMap = Map.Make(String)
type gamma = typ StringMap.t

let is_equalable = function
  | TyInt    -> true
  | TyStr    -> true
  | TyList _ -> true
  | TyTup  _ -> true
  | _        -> false

let is_comparable = function
  | TyInt -> true
  | TyStr -> true
  | _     -> false

let is_numeric = function
  | TyInt -> true
  | _     -> false

let is_addable = function
  | TyInt    -> true
  | TyStr    -> true
  | TyList _ -> true
  | _        -> false

let valid_types_for_infix (a : typ) (b : typ) = function
  | Add     -> is_addable    a && is_addable    b
  | Sub     -> is_numeric    a && is_numeric    b
  | Mul     -> is_numeric    a && is_numeric    b
  | Div     -> is_numeric    a && is_numeric    b
  | Mod     -> is_numeric    a && is_numeric    b
  | Less    -> is_comparable a && is_comparable b
  | Greater -> is_comparable a && is_comparable b
  | Leq     -> is_comparable a && is_comparable b
  | Geq     -> is_comparable a && is_comparable b
  | Eq      -> is_equalable  a && is_equalable  b
  | Neq     -> is_equalable  a && is_equalable  b
  | Or      -> is_numeric    a && is_numeric    b
  | And     -> is_numeric    a && is_numeric    b

module StringSet = Set.Make(String)
let free_vars (e : expr) =
  let rec f (s : StringSet.t) = function
    | Id nm -> StringSet.add nm s
    | LitInt _ -> s
    | LitStr _ -> s
    | LitList es -> f_list s es
    | LitTup es -> f_list s es
    | Infix (a, _, b) -> f_list s [a; b]
    | Bind (_, nm, v, rest) ->
       let s' = f_list s [v; rest] in
       StringSet.remove nm s'
    | Assign (_, v, rest) -> f_list s [v; rest]
    | Fun (_, nm, params, body, rest) ->
       let param_nms = List.map snd params in
       let free_in_body = StringSet.diff (f StringSet.empty body)
                            (StringSet.of_list param_nms) in
       StringSet.remove nm (StringSet.union (f s rest) free_in_body)
    | App (fe, arg_es) -> f_list s (fe::arg_es)
    | Cont (_, nm, alpha, body, rest) ->
       let free_in_body = StringSet.remove alpha (f StringSet.empty body) in
       StringSet.remove nm (StringSet.union (f s rest) free_in_body)
    | If (a, b, c) -> f_list s [a; b; c]

  and f_list (s : StringSet.t) (es : expr list) =
    List.fold_left (fun set expr ->
        StringSet.union set (f StringSet.empty expr))
      s es in

  f StringSet.empty e

let rec replace_identifier(old_name : string) (new_name : string) (s : sexpr) =
  let f = replace_identifier old_name new_name in
  match s with
  | typ, SId nm when nm = old_name -> typ, SId new_name
  | typ, SId nm -> typ, SId nm
  | typ, SLitInt i -> typ, SLitInt i
  | typ, SLitStr s -> typ, SLitStr s
  | typ, SLitList es -> typ, SLitList (List.map f es)
  | typ, SLitTup es -> typ, SLitTup (List.map f es)
  | typ, SInfix (a, op, b) -> typ, SInfix (f a, op, f b)
  | typ, SBind (t, nm, v, rest) ->
     let v' = f v in
     if nm = new_name then
       typ, SBind (t, nm, v', rest)
     else
       typ, SBind (t, nm, v', f rest)
  | typ, SAssign (nm, v, rest) when nm = old_name -> typ, SAssign (new_name, f v, f rest)
  | typ, SAssign (nm, v, rest) -> typ, SAssign (nm, f v, f rest)
  | typ, SFun finfo ->
     if finfo.name = new_name then
       typ, SFun finfo
     else
       typ, SFun { finfo with body = f finfo.body; rest = f finfo.rest }
  | typ, SApp (fe, arg_es) -> typ, SApp (f fe, List.map f arg_es)
  | typ, SCont (t, nm, alpha, body, rest) ->
     typ, (match nm = new_name, alpha = new_name with
           | false, false -> SCont (t, nm, alpha, f body, f rest)
           | false, true  -> SCont (t, nm, alpha,   body, f rest)
           | true,  false -> SCont (t, nm, alpha, f body,   rest)
           | true,  true  -> SCont (t, nm, alpha,   body,   rest))
  | typ, SIf (a, b, c) -> typ, SIf (f a, f b, f c)

let rec expr_to_sexpr (g : gamma) (e : expr) : sexpr =
  let rec ty (e : expr) =
    match e with
    | Id "curr_clo" -> raise (TypeError (e, "curr_clo is a reserved name"))
    | Id nm ->
       (match StringMap.find_opt nm g with
        | None -> raise (TypeError (e, nm ^ " is unbound"))
        | Some ty -> ty, SId nm)
    | LitInt i -> TyInt, SLitInt i
    | LitStr s -> TyStr, SLitStr s
    | LitList [] ->
       raise (TypeError (e, "empty list literals must be explicity bound"))
    | LitList es ->
       let ses = List.map ty es in
       let fst_ty = fst (List.hd ses) in
       (if not (List.for_all (fun (ty, _) -> ty = fst_ty) ses) then
          raise (TypeError (e, "list literals must by type-homogeneous")));
       TyList fst_ty, SLitList ses
    | LitTup es ->
       let ses = List.map ty es in
       let tys = List.map fst ses in
       TyTup tys, SLitTup ses
    | Infix (a, op, b) ->
       let sa = ty a in
       let sb = ty b in
       (if not (fst sa = fst sb) then
          raise (TypeError (e, "lhs and rhs of infix expressions must have"
                               ^ " the same type")));
       (if not (valid_types_for_infix (fst sa) (fst sb) op) then
          raise (TypeError (e, op_str op ^ " does not support "
                               ^ typ_str (fst sa) ^ " operands")));
       fst sa, SInfix (sa, op, sb)
    | Bind (typ, nm, e, rest) ->
       let se = ty e in
       (if not (typ = fst se) then
          raise (TypeError (e, nm ^ " has type " ^ typ_str typ
                               ^ " but it is assigned to a value of type "
                               ^ typ_str (fst se))));
       let g' = StringMap.add nm (fst se) g in
       let rest_ty, _ as rest_sexpr = expr_to_sexpr g' rest in
       rest_ty, SBind (typ, nm, se, rest_sexpr)
    | Assign (nm, e, rest) ->
       let ty_nm =
         match StringMap.find_opt nm g with
         | None -> raise (TypeError (e, "cannot assign to unbound variable " ^ nm))
         | Some ty -> ty in
       let se = ty e in
       (if not (ty_nm = fst se) then
          raise (TypeError (e, nm ^ " has type " ^ typ_str ty_nm
                               ^ " but it is assigned to a value of type "
                               ^ typ_str (fst se))));
       let rest_ty, _ as rest_sexpr = ty rest in
       rest_ty, SAssign (nm, se, rest_sexpr)
    | Fun (typ, name, parameters, body, rest) ->
       let param_names = List.map snd parameters in
       let free_vars_in_body = StringSet.diff (free_vars body)
                                 (StringSet.of_list param_names) in
       let free_vars_in_body = StringSet.diff free_vars_in_body
                                 (StringSet.of_list ["p"]) in
       let free_vars_array = Array.of_list (StringSet.fold (fun v list -> v::list)
                                              free_vars_in_body []) in
       let f_type = TyFun(List.map (fun (ty, _) -> ty) parameters, typ) in
       let gamma' = List.fold_left (fun env (ty, nm) ->
                        StringMap.add nm ty env) g
                      ((f_type, name)::parameters) in
       let body_sexpr = expr_to_sexpr gamma' body in
       (* replace every free variable in the body of the function
        * with its index in free_vars_array *)
       let _, body_sexpr' =
         Array.fold_left (fun (idx, e) free_var ->
             idx + 1, replace_identifier free_var (Int.to_string idx) e)
           (0, body_sexpr) free_vars_array in
       if not (typ = fst body_sexpr) then
         raise (TypeError (e, name ^ "'s body has type "
                              ^ typ_str (fst body_sexpr)
                              ^ " but " ^ typ_str typ
                              ^ " was expected"));
       (* gamma'' is the env that rest will be checked in *)
       let gamma'' = StringMap.add name f_type g in
       let s_rest = expr_to_sexpr gamma'' rest in
       fst s_rest, SFun { ret_t = typ;
                          name;
                          parameters;
                          body = body_sexpr';
                          rest = s_rest;
                          llvm_decl = None;
                          free_variables = free_vars_array }
    | App (Id "p", args) ->
       (match args with
        | [arg] ->
           let sa = ty arg in
           TyInt, SApp ((TyFun ([fst sa], TyInt), SId "p"), [sa])
        | _ -> raise (TypeError (e, "p may only be applied with 1 argument")))
    | App (f, args) ->
       let sf = ty f in
       let s_args = List.map ty args in
       (match fst sf with
        | TyFun (args, ret_ty) as f_typ ->
           if not (List.length args = List.length s_args) then
             raise (TypeError (e, "function of type " ^ typ_str f_typ
                                  ^ " was applied with incorrect number"
                                  ^ " of arguments (expected "
                                  ^ Int.to_string (List.length args) ^ " but got "
                                  ^ Int.to_string (List.length s_args)));
           (List.iter2 (fun arg_type (arg_type', _) ->
                if not (arg_type = arg_type') then
                  raise (TypeError (e, "function of type " ^ typ_str f_typ
                                       ^ " was applied with an argument of "
                                       ^ "type " ^ typ_str arg_type'
                                       ^ " but " ^ typ_str arg_type ^ " was expected"))
              ) args s_args);
           ret_ty, SApp(sf, s_args)
        | _ -> raise (TypeError (e, "non-function cannot be applied")))
    | Cont (_typ, _name, _alpha, _body, _rest) -> raise (Unimplemented "ty Cont")
    | If (test_e, true_e, false_e) ->
       let s_test_e = ty test_e in
       let s_true_e = ty true_e in
       let s_false_e = ty false_e in
       if not (fst s_test_e = TyInt) then
         raise (TypeError (e, expr_string test_e ^ " does not have type int"));
       if not (fst s_true_e = fst s_false_e) then
         raise (TypeError (e, "both branches of an if expression must "
                              ^ "have the same type"));
       fst s_true_e, SIf(s_test_e, s_true_e, s_false_e)
  in ty e
