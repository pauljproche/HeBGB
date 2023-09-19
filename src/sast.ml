open Ast

type sx = 
    SId      of string
  | SLitInt  of int
  | SLitStr  of string
  | SLitList of sexpr list
  | SLitTup  of sexpr list
  | SInfix   of sexpr * op * sexpr
  | SBind    of typ * string * sexpr * sexpr
  | SAssign  of string * sexpr * sexpr
  | SFun     of {
      ret_t : typ;
      name: string;
      parameters : (typ * string) list;
      body: sexpr;
      rest : sexpr;
      llvm_decl : Llvm.llvalue option;
      free_variables : string array }
  | SApp     of sexpr * sexpr list
  | SCont    of typ * string * string * sexpr * sexpr
  | SIf      of sexpr * sexpr * sexpr

and sexpr = Ast.typ * sx

let sexpr_string (s : sexpr) =
  let rec f (s : sexpr) (indent : string) =
    let t, sx = s in
    let type_string = Ast.typ_str t in
    let label, children = match sx with
      | SId s -> "Id (" ^ s ^ ")", []
      | SLitInt i -> "Int (" ^ Int.to_string i ^ ")", []
      | SLitStr s -> "Str (" ^ s ^ ")", []
      | SLitList es -> "List", es
      | SLitTup es -> "Tuple", es
      | SInfix (a, op, b) -> "Infix (" ^ Ast.op_str op ^ ")", [a; b]
      | SBind (typ, nm, v, body) -> "Bind (" ^ Ast.typ_str typ ^ " " ^ nm ^ ")",
                                    [v; body]
      | SAssign (nm, v, body) -> "Assign (" ^ nm ^ ")", [v; body]
      | SFun finfo ->
         "Fun " ^ finfo.name ^ " returns: " ^ Ast.typ_str finfo.ret_t
         ^ " args: " ^ Ast.args_str finfo.parameters,
         [finfo.body; finfo.rest ]
      | SApp (fe, args) -> "App", fe::args
      | SCont (t, nm, alpha, v, body) -> "Cont (" ^ nm ^ ") returns: "
                                         ^ Ast.typ_str t ^ " with: " ^ alpha,
                                         [v; body]
      | SIf (a, b, c) -> "If", [a; b; c] in
    let rec child_string = function
      | [] -> ""
      | [e] -> indent ^ "+-- "
               ^ f e (indent ^ "    ")
      | e::rest -> indent ^ "|-- "
                   ^ f e (indent ^ "|   ")
                   ^ child_string rest in
    "[" ^ type_string ^ "] " ^ label ^ "\n" ^ child_string children in
  f s ""
