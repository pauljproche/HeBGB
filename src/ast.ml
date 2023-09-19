(* ast.ml
 *
 * HeBGB Programming Language
 *)


type typ = 
    TyInt                    (* type of integers            *)
  | TyStr                    (* type of strings             *)
  | TyCont                   (* type of continuation values *)
  | TyList of typ            (* type of lists               *)
  | TyTup  of typ list       (* type of tuples              *)
  | TyFun  of typ list * typ (* type of functions           *)

type op = Add | Sub | Mul | Div | Mod | Less | Greater | Leq | Geq | Eq | Neq | Or | And

type expr = 
    Id      of string
  | LitInt  of int
  | LitStr  of string
  | LitList of expr list
  | LitTup  of expr list
  | Infix   of expr * op * expr
  | Bind    of typ * string * expr * expr
  | Assign  of string * expr * expr
  | Fun     of typ * string * ((typ * string) list) * expr * expr
  | App     of expr * expr list
  | Cont    of typ * string * string * expr * expr
  | If      of expr * expr * expr

type program = expr

let rec typ_str = function
    TyInt -> "INT"
  | TyStr -> "STRING"
  | TyCont -> "CONT"
  | TyList(ty) -> typ_str ty ^ " LIST"
  | TyTup(tys) -> "(" ^ String.concat "*" (List.map typ_str tys) ^ ")"
  | TyFun(tys, ret_t) -> "(" ^ String.concat ", " (List.map typ_str tys) ^") -> "
                         ^ typ_str ret_t

let op_str = function
    Add -> " + "    
  | Sub -> " - "
  | Mul -> " x " (* FIXME: fix test runner to allow printing * *)
  | Div -> " / "
  | Mod -> " % "
  | Less -> " < "
  | Greater -> " > "
  | Leq -> " <= "
  | Geq -> " => "
  | Eq -> " == "
  | Neq -> " != "
  | Or -> " || "
  | And -> " && "

let rec args_str args = String.concat ", " (List.map (fun (ty, e) -> typ_str ty ^ " " ^ e) args)
and params_str params = String.concat ", " (List.map expr_string params)

and expr_string (e : expr) =
  let rec f (e : expr) (indent : string) =
    let label, children = match e with
      | Id s -> "Id (" ^ s ^ ")", []
      | LitInt i -> "Int (" ^ Int.to_string i ^ ")", []
      | LitStr s -> "Str (" ^ s ^ ")", []
      | LitList es -> "List", es
      | LitTup es -> "Tuple", es
      | Infix (a, op, b) -> "Infix (" ^ op_str op ^ ")", [a; b]
      | Bind (typ, nm, v, body) -> "Bind (" ^ typ_str typ ^ " " ^ nm ^ ")",
                                    [v; body]
      | Assign (nm, v, body) -> "Assign (" ^ nm ^ ")", [v; body]
      | Fun (ret_t, name, parameters, body, rest) ->
         "Fun " ^ name ^ " returns: " ^ typ_str ret_t
         ^ " args: " ^ args_str parameters,
         [body; rest]
      | App (fe, args) -> "App", fe::args
      | Cont (t, nm, alpha, v, body) -> "Cont (" ^ nm ^ ") returns: "
                                         ^ typ_str t ^ " with: " ^ alpha,
                                         [v; body]
      | If (a, b, c) -> "If", [a; b; c] in
    let rec child_string = function
      | [] -> ""
      | [e] -> indent ^ "+-- "
               ^ f e (indent ^ "    ")
      | e::rest -> indent ^ "|-- "
                   ^ f e (indent ^ "|   ")
                   ^ child_string rest in
    label ^ "\n" ^ child_string children in
  f e ""

let to_string e =
  expr_string e ^ "\n"
