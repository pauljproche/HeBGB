module StringMap = Map.Make(String)

let () =
  let usage_msg = "usage: ./hebgb.native [file.hebgb]" in
  let print_ast = ref false in
  let print_sast = ref false in
  let nocodegen = ref false in
  let speclist = [("-A", Arg.Set print_ast,  "Print the AST");
                  ("--nocodegen", Arg.Set nocodegen, "Don't generate output code");
                  ("-S", Arg.Set print_sast, "Print the SAST")] in
  let channel = ref stdin in
  Arg.parse speclist (fun file -> channel := open_in file) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = try Parser.program Scanner.token lexbuf with
            | Stdlib.Parsing.Parse_error ->
               print_endline "Parse Error!";
               ignore (exit 1); (Ast.Id "dummy") in
  if !print_ast then print_endline (Ast.expr_string ast);
  let sast = try Semant.expr_to_sexpr StringMap.empty ast with
             | Semant.TypeError (expr, msg) ->
                print_endline "Error!";
                print_endline (Ast.expr_string expr);
                print_endline msg;
                ignore (exit 1); (Ast.TyInt, Sast.SId "dummy") in
  if !print_sast then print_endline (Sast.sexpr_string sast);
  if !nocodegen then ()
  else
    let llvm = Llvm.string_of_llmodule (Codegen.translate sast) in
    print_endline llvm
