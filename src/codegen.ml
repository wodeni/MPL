(*
 * File: codegen.ml
 * Date: 2017-03-13
 *
 * PLT Spring 2017
 * MPL Project
 * Wode "Nimo" Ni    <wn2155@columbia.edu>
 * David Rincon-Cruz <dr2884@columbia.edu>
 * Chi Zhang         <cz2440@columbia.edu>
 * Jiangfeng Wang    <jw3107@columbia.edu>
 *)

open Exceptions
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)
module MatrixMap = Map.Make(String)

let translate (functions) =
  let context    = L.global_context () in
  let the_module = L.create_module context "MPL"
  and i32_t      = L.i32_type  context
  and i8_t       = L.i8_type   context
  and i1_t       = L.i1_type   context
  and void_t     = L.void_type context
  and float_t    = L.double_type context
  and array_t    = L.array_type
  and pointer_t  = L.pointer_type
  in

  let matrix_int_t m n   = array_t (array_t i32_t m) n 
  and matrix_float_t m n = array_t (array_t float_t m) n in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.Void -> void_t 
    | A.Mat(typ, rows, cols) ->
            (*
        let rows' = match rows with A.IntLit(s) -> s | _ -> raise(Exceptions.InvalidMatrixDimension) in
        let cols' = match cols with A.IntLit(s) -> s | _ -> raise(Exceptions.InvalidMatrixDimension) in
  *)
        (match typ with
                A.Int     -> array_t (array_t i32_t cols) rows
                | A.Float -> array_t (array_t float_t cols) rows
                | _ -> raise(Exceptions.UnsupportedMatrixType)        )
    | _ -> raise(Exceptions.UnsupportedType)
  in

  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* TODO: Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  let printm_int_t = L.function_type i32_t [| L.pointer_type i8_t; i32_t; i32_t |] in
  let printm_int_func = L.declare_function "printm_int" printm_int_t the_module in

  let printm_float_t = L.function_type i32_t [| L.pointer_type i8_t; float_t; float_t |] in
  let printm_float_func = L.declare_function "printm_float" printm_float_t the_module in

  (* Define each function so we can call it *)
  (* NOTE: We only have one argument, and it has the same type of the return type *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) [| (ltype_of_typ fdecl.A.typ) |] in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder           = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str    = L.build_global_stringptr "%d\n" "fmti" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmts" builder in


    let get_mat_dimensions t = match t with
        A.Mat(typ, rows, cols) -> (typ, rows, cols)
        | _                    -> raise ( UnsupportedMatrixType ) 
    in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
        let add_formal m (t, n) p = L.set_value_name n p;
            let local = L.build_alloca (ltype_of_typ t) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m in
	let add_local (m,mat_m) (t, n) =  
		let local_var = L.build_alloca (ltype_of_typ t) n builder in
		(match t with
			A.Mat(typ, row, cols) -> 
                let dim = get_mat_dimensions t in 
                ((StringMap.add n local_var m),(StringMap.add n dim mat_m)) 
			| _ -> ((StringMap.add n local_var m), mat_m)) 
    in

    (* NOTE: We do not have any argument. Might not need this
     * NOTE: For entry functions - do we put the neighbors in the formal
     * , or the matrix and the location of the entry??
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
    *)

    (* Add the local variables to a new map *)
    List.fold_left add_local (StringMap.empty,MatrixMap.empty) fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    (* TODO: should we dthrow exception here? *)
    let getSlocal (a,_) = a in
    let getMlocal (_,b) = b in
    let lookup n = try StringMap.find n (getSlocal local_vars)
                   with Not_found ->  raise(Exceptions.LocalNotFound)
                       (* raise (Error "unknown variable name") *)
    in 

    let find_matrix_type matrix =
      match (List.hd (List.hd matrix)) with
        A.IntLit _   -> ltype_of_typ (A.Int)
      | A.FloatLit _ -> ltype_of_typ (A.Float)
      | A.BoolLit _  -> ltype_of_typ (A.Bool)
      | _            -> raise (UnsupportedMatrixType) in

    let idx n m = [| L.const_int i32_t n; L.const_int i32_t m |] in
    let lookupM t =
        match t with 
        A.Id(s) -> MatrixMap.find s (getMlocal local_vars)
        | _ -> raise(Exceptions.UnsupportedMatrixType) 
    in
    let lookup_matrixid t =  
        match t with 
        A.Id(s) -> lookup s 
        | _ -> raise(Exceptions.UnsupportedMatrixType) 
    in   

    (* Build instructions for apply operation, this will translate a single 
     * apply to 9 distinct llvm function calls. 
     * @fname = string of the function name
     * @mat = loaded llvalue that is a matrix *)
    (* TODO
    let build_apply f mat n b = 
         let (fdef, fdecl) = StringMap.find f function_decls in
         let actuals = List.rev (List.map (expr builder) (List.rev act)) in
         let result = f ^ "_result" in
         L.build_call fdef (Array.of_list actuals) result builder 
    in
    *)

    let build_matrix_access i j s rows cols builder assign =
        if ((rows < i) && (cols > j)) then raise(Exceptions.MatrixOutOfBoundsAccess(""));
        if assign
            then L.build_gep (lookup s) (idx i j) s builder
           (* FIXME: it only gets the row, not what we want. *)
        else L.build_load (L.build_gep (lookup s) (idx i j)  s builder) s builder
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder expression =  match expression with
	A.IntLit(i)       -> L.const_int i32_t i
	  | A.FloatLit(i) -> L.const_float float_t i
      | A.BoolLit b   -> L.const_int i1_t (if b then 1 else 0)
      | A.StrLit s    -> L.build_global_stringptr s ("str_" ^ s) builder
      | A.MatrixLit l -> let i64Lists        = List.map (List.map (expr builder)) l in
                        let listOfArrays    = List.map Array.of_list i64Lists in
                        let i64ListOfArrays = List.rev (List.map (L.const_array (find_matrix_type l)) listOfArrays) in
                        let arrayOfArrays   = Array.of_list i64ListOfArrays in
              L.const_array (array_t (find_matrix_type l)(List.length (List.hd l))) arrayOfArrays
      | A.Noexpr      -> L.const_int i32_t 0
      | A.Id s        -> L.build_load (lookup s) s builder (* lookup s *)
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
          (match op with
            A.Add     -> L.build_add e1' e2' "tmp" builder
          | A.Sub     -> L.build_sub e1' e2' "tmp" builder
          | A.Mult    -> L.build_mul e1' e2' "tmp" builder
          | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
          | A.And     -> L.build_and e1' e2' "tmp" builder
          | A.Or      -> L.build_or e1' e2' "tmp" builder
          | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
          | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
          | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
          | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
          | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
          (*
          | A.Apply   -> build_apply e1 e2' builder
           TODO: EMult, EDiv, Matapply *)
          ) 
      | A.Unop(op, e) ->
          let e' = expr builder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) 
          e' "tmp" builder
      | A.MatrixAccess (s, i, j) ->
      (*
            let i = expr builder row and j = expr builder col in
            let access_i = (match row with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1)
            and access_j = (match col with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1) in
      *)
		let (typ, rows, cols) = MatrixMap.find s (getMlocal local_vars) in
            (build_matrix_access i j s rows cols builder false)
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	    L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | A.Call ("prints", [e]) -> 
	    L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
      | A.Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call ("printm", [e]) ->
		let (typ, rows, cols) = (lookupM e) in
        let id = lookup_matrixid e in
        let id_ptr = L.build_in_bounds_gep id (idx 0 0) "build_in_bounds_gep" builder in 
        let mat_ptr = L.build_bitcast id_ptr (pointer_t i8_t) "mat_ptr" builder 
                in (match typ with
                            A.Int -> L.build_call printm_int_func [| mat_ptr;(L.const_int i32_t rows); (L.const_int i32_t cols) |] "printm_int" builder
                            | A.Float -> L.build_call printm_float_func [| mat_ptr; (L.const_int i32_t rows); (L.const_int i32_t cols) |] "printm_float" builder
                            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
                        
      (* NOTE: we do not have any user defined functions
       *       Will use this code once we implement "@"
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    *)
      | _ -> raise(Exceptions.StatementNotSuuported)
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in


    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
