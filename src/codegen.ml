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
open Printf
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
  and matrix_float_t m n = array_t (array_t float_t m) n 
  and matrix_t t m n   = array_t (array_t t m) n 
in
 
  let rec func_ptr_t typ = 
      let arr  = Array.make 9 (ltype_of_typ typ) in
      let ftype = L.function_type (ltype_of_typ typ) arr in
      pointer_t ftype 
  and
       
  ltype_of_typ = function
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
    | A.FMat(typ, rows, cols) -> array_t (array_t   (func_ptr_t typ) cols) rows
    | _ -> raise(Exceptions.UnsupportedType)
  in

  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  let printm_int_t = L.function_type i32_t [| L.pointer_type i8_t; i32_t; i32_t |] in
  let printm_int_func = L.declare_function "printm_int" printm_int_t the_module in
  let printm_float_t = L.function_type i32_t [| L.pointer_type i8_t; float_t; float_t |] in
  let printm_float_func = L.declare_function "printm_float" printm_float_t the_module in

  let matread_int_t = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let matread_int_func = L.declare_function "matread_int" matread_int_t the_module in
  let matread_float_t = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let matread_float_func = L.declare_function "matread_float" matread_float_t the_module in

  let matwrite_int_t = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let matwrite_int_func = L.declare_function "matwrite_int" matread_int_t the_module in
  let matwrite_float_t = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let matwrite_float_func = L.declare_function "matwrite_float" matread_float_t the_module in


  (* Define each function so we can call it *)
  (* NOTE: We only have one argument, and it has the same type of the return type *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname in 
      let arr  = Array.make 9 (ltype_of_typ fdecl.A.typ) in
      let ftype = 
          if(name = "main") then 
              L.function_type (ltype_of_typ fdecl.A.typ) [| (ltype_of_typ fdecl.A.typ) |]
          else 
              L.function_type (ltype_of_typ fdecl.A.typ) arr 
          in
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

    (* An array of string representation of the 9 neighbhors *)
    let typ = fdecl.A.typ in
    let neighbor_names = [ "#NW"; "#N"; "#NE"; "#W"; "#C"; "#E"; "#SW"; "#S"; "#SE" ]
    in
    let neighbor_list = List.map (fun x -> (typ, x)) neighbor_names
    in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
        let add_formal m (t, n) p = L.set_value_name n p;
            let local = L.build_alloca (ltype_of_typ t) ("sharp" ^ n) builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m in
	(*Note*)
	let add_local (m,mat_m) (t, n) =  
		let local_var = L.build_alloca (ltype_of_typ t) n builder in
		(match t with
			A.Mat(typ, row, cols) -> 
                let dim = get_mat_dimensions t in 
                ((StringMap.add n local_var m),(StringMap.add n dim mat_m)) 
			| _ -> ((StringMap.add n local_var m), mat_m)) 
    in

    let formals = 
        if(fdecl.A.fname <> "main") then 
            (List.fold_left2 add_formal StringMap.empty neighbor_list
            (Array.to_list (L.params the_function))) 
        else StringMap.empty in
        

    (* NOTE: We do not have any argument. Might not need this
     * NOTE: For entry functions - do we put the neighbors in the formal
     * , or the matrix and the location of the entry??
    *)

    (* Add the local variables to a new map *)
    List.fold_left add_local (formals,MatrixMap.empty) fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    (* TODO: should we dthrow exception here? *)
    let getSlocal (a,_) = a in
    let getMlocal (_,b) = b in
    let lookup n = try StringMap.find n (getSlocal local_vars) with Not_found ->  print_endline(n);raise(Exceptions.LocalNotFound("unknown variable name: "^n)) 
(*  
 *raise (Error ("unknown variable name: "^n) *)
    in 

    let find_matrix_type matrix =
      match (List.hd (List.hd matrix)) with
        A.IntLit _   -> ltype_of_typ (A.Int)
      | A.FloatLit _ -> ltype_of_typ (A.Float)
      | A.BoolLit _  -> ltype_of_typ (A.Bool)
      | A.Id s -> let func_type = L.type_of (fst (StringMap.find s function_decls))
		in pointer_t func_type 
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
    let get_string_by_id = function
        | Ast.Id(s) -> s
        | _ -> raise(Exceptions.UnsupportedMatrixType)  (* TODO *)
    in

    let build_matrix l expr builder =
        let i64Lists        = List.map (List.map (expr builder)) l in
        let listOfArrays    = List.map Array.of_list i64Lists in
        let i64ListOfArrays = (List.map (L.const_array (find_matrix_type l)) listOfArrays) in
        let arrayOfArrays   = Array.of_list i64ListOfArrays in
        L.const_array (array_t (find_matrix_type l)(List.length (List.hd l))) arrayOfArrays
    in

    let build_matrix_access i j s rows cols builder assign =
        if ((i >= rows) || (j >= cols)) then raise(Exceptions.MatrixOutOfBoundsAccess(""));
        if assign
            then L.build_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t i;L.const_int i32_t j|] s builder
        else L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t i;L.const_int i32_t j|]  s builder) s builder
    in
    (* Build instructions for apply operation, this will translate a single 
     * apply to 9 distinct llvm function calls. 
     * @fname = string of the function name
     * @mat = loaded llvalue that is a matrix *)
    let build_apply f_expr mat n b = 
	let (typ, rows, cols) = (lookupM mat) in
        let mat_str = get_string_by_id mat in
        let f = get_string_by_id f_expr in
        let (fdef, fdecl) = StringMap.find f function_decls in
        let result = f ^ "_result" in
        (* FIXME
        let new_mat_l = [] in
            for i=0 to (rows-1) do
                new_mat_l 
                for j=0 to (cols-1) do
                done
            done
        in
        *)
        (* Loop through all elements in the matrix *)
       (* 
	let blist = [] in
        for i=0 to (rows-1) do
            for j=0 to (cols-1) do
		b :: blist	
            done
	done;
*)
(*
	let entrylist = [] in
        for i=0 to (rows-1) do
            for j=0 to (cols-1) do
                let entryp = build_matrix_access i j mat_str rows cols b true in
		entryp :: entrylist	
            done
	done;
*)
	let arrMat = Array.make (rows*cols) (L.const_int i32_t 0) in 
	for i = 0 to (rows-1) do
	    for j = 0 to (cols-1) do
		arrMat.(i*cols+j) <- (build_matrix_access i j mat_str rows cols b false)
	    done	
	done;
	for i=0 to (rows-1) do
            for j=0 to (cols-1) do
                let entry = build_matrix_access i j mat_str rows cols b true in
		let arr = Array.make 9 (L.const_int i32_t 0) in
                (* for all the nine neighbors *)
                for m=i-1 to (i+1) do
                    for n=j-1 to (j+1) do
                        let index = 3 * (m - (i - 1)) + (n - (j - 1)) in
                        if ((m < rows) && (n < cols) && (m >= 0) && (n >= 0)) then 
                            arr.(index) <- arrMat.(m*cols+n)
                        else arr.(index) <- (L.const_int i32_t 0)
                    done
                done;
            	let res = L.build_call fdef arr result b in
		L.build_store res entry b
	    done
        done;
		
        L.build_load (L.build_gep (lookup mat_str) [| L.const_int i32_t 0 |] n b) n b
    in 
let find_fptr_by_id typ builder = function
A.Id id ->  L.build_bitcast (fst (StringMap.find id function_decls)) typ "func_ptr" builder 
| _ -> raise(Exceptions.UnsupportedMatrixType)      
in
(*
    let buildName s = String.sub s 1 ((String.length s)-2) 
    in*)  
    (* Construct code for an expression; return its value *)
    let rec expr builder expression =  match expression with
        A.IntLit(i)       -> L.const_int i32_t i
	  | A.FloatLit(i) -> L.const_float float_t i
      | A.BoolLit b   -> L.const_int i1_t (if b then 1 else 0)
      | A.StrLit s    -> L.build_global_stringptr s "str_lit" builder
      | A.MatrixLit l -> build_matrix l expr builder
      | A.Noexpr      -> L.const_int i32_t 0
      | A.Id s        -> L.build_load (lookup s) s builder (* lookup s *)
      | A.Binop (e1, op, e2) ->
          (match op with
            A.Add     -> L.build_add (expr builder e1) (expr builder e2) "tmp" builder
          | A.Sub     -> L.build_sub (expr builder e1) (expr builder e2) "tmp" builder
          | A.Mult    -> L.build_mul (expr builder e1) (expr builder e2) "tmp" builder
          | A.Div     -> L.build_sdiv (expr builder e1) (expr builder e2) "tmp" builder
          | A.And     -> L.build_and (expr builder e1) (expr builder e2) "tmp" builder
          | A.Or      -> L.build_or (expr builder e1) (expr builder e2) "tmp" builder
          | A.Equal   -> L.build_icmp L.Icmp.Eq (expr builder e1) (expr builder e2) "tmp" builder
          | A.Neq     -> L.build_icmp L.Icmp.Ne (expr builder e1) (expr builder e2) "tmp" builder
          | A.Less    -> L.build_icmp L.Icmp.Slt (expr builder e1) (expr builder e2) "tmp" builder
          | A.Leq     -> L.build_icmp L.Icmp.Sle (expr builder e1) (expr builder e2) "tmp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt (expr builder e1) (expr builder e2) "tmp" builder
          | A.Geq     -> L.build_icmp L.Icmp.Sge (expr builder e1) (expr builder e2) "tmp" builder
          | A.Apply   -> build_apply e1 e2 "tmp"  builder
          (* TODO: EMult, EDiv, Matapply *)
          ) 
      | A.Unop(op, e) ->
          let e' = expr builder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) 
          e' "tmp" builder
      | A.MatrixAccess (s, i, j) ->
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
                            A.Int -> L.build_call printm_int_func [| mat_ptr;(L.const_int i32_t rows); 
                                (L.const_int i32_t cols) |] "printm_int" builder
                            | A.Float -> L.build_call printm_float_func [| mat_ptr; (L.const_int i32_t rows); 
                                (L.const_int i32_t cols) |] "printm_float" builder
                            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | A.Call ("matwrite", [e1; e2]) ->
		let (typ, rows, cols) = (lookupM e2) in
        let id = lookup_matrixid e2 in
        let id_ptr = L.build_in_bounds_gep id (idx 0 0) "build_in_bounds_gep" builder in 
        let mat_ptr = L.build_bitcast id_ptr (pointer_t i8_t) "mat_ptr" builder 
            in (match typ with
                A.Int -> L.build_call matwrite_int_func [| (expr builder e1); mat_ptr;
                    (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matwrite_int" builder
                | A.Float -> L.build_call matwrite_float_func [| (expr builder e1); mat_ptr;
                    (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matwrite_float" builder
                | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | A.Call ("matread", [e1; e2])  ->
		let (typ, rows, cols) = (lookupM e2) in
        let id = lookup_matrixid e2 in
        let id_ptr = L.build_in_bounds_gep id (idx 0 0) "build_in_bounds_gep" builder in 
        let mat_ptr = L.build_bitcast id_ptr (pointer_t i8_t) "mat_ptr" builder 
            in (match typ with
                A.Int -> L.build_call matread_int_func [| (expr builder e1); mat_ptr;
                    (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matread_int" builder
                | A.Float -> L.build_call matread_float_func [| (expr builder e1); mat_ptr;
                    (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matread_float" builder
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
