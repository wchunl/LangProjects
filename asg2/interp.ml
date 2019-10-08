(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)
(* Wai Chun Leung *)
(* CruzID: 1611864 *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with 
          | Arrayref (ident, expr) -> 
               let arr = Hashtbl.find Tables.array_table ident in
               let evaled = eval_expr expr in 
               let evaledInt = int_of_float evaled in 
               Array.get arr (evaledInt - 1)
          | Variable ident ->  
               Hashtbl.find Tables.variable_table ident)
    | Unary (oper, expr) -> 
          let evaled = eval_expr expr in 
          let op = Hashtbl.find Tables.unary_fn_table oper in
          op evaled
    | Binary (oper, expr1, expr2) -> 
          let evaled1 = eval_expr expr1 in
          let evaled2 = eval_expr expr2 in
          let op = Hashtbl.find Tables.binary_fn_table oper in
          op evaled1 evaled2

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ()); None

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref = match memref with
          | Arrayref (ident, expr) -> print_string ("Error"); exit 0;
          | Variable ident -> 
               try  let number = Etc.read_number ()
                    in Hashtbl.add Tables.variable_table ident number
               with End_of_file -> 
                    (Hashtbl.add Tables.variable_table "eof" 1.;
                    let n = Hashtbl.find Tables.variable_table "nan" in
                    Hashtbl.add Tables.variable_table ident n)
               in List.iter input_number memref_list; None

let interp_let (memref : Absyn.memref) (expr1 : Absyn.expr) = 
     match memref with
     | Arrayref (ident, expr2) -> 
          let arr = Hashtbl.find Tables.array_table ident in
          let evaled1 = eval_expr expr1 in
          let evaled2 = eval_expr expr2 in
          let evaled2Int = int_of_float evaled2 in
          Array.set arr (evaled2Int - 1) evaled1; None
     | Variable ident -> 
          let evaled = eval_expr expr1 in 
          Hashtbl.add Tables.variable_table ident evaled; None

let interp_if (expr : Absyn.expr) (label : Absyn.label) = 
     match expr with
     | Binary (oper, expr1, expr2) -> 
          let evaled1 = eval_expr expr1 in
          let evaled2 = eval_expr expr2 in
          let boolOp = Hashtbl.find Tables.boolean_op_table oper in
          let result =  boolOp evaled1 evaled2 in
          if result then Some (Hashtbl.find Tables.label_table label)
          else None
     | _ -> print_string ("Error"); exit 0

let interp_dim (ident : Absyn.ident) (expr : Absyn.expr) =
     let evaled = eval_expr expr in
     let evaledInt = int_of_float evaled in
     let arr = Array.make evaledInt 0. in
     Hashtbl.add Tables.array_table ident arr; None

let  interp_stmt (stmt : Absyn.stmt) = match stmt with
    | Dim (ident, expr) -> interp_dim ident expr
    | Let (memref, expr) -> interp_let memref expr
    | Goto label -> Some (Hashtbl.find Tables.label_table label)
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list
    | Input memref_list -> interp_input memref_list

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> let next_line = interp_stmt stmt in 
                           match next_line with
                           | None -> interpret otherlines
                           | Some line -> interpret line

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

