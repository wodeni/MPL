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
open Sast
open Semant
open Exceptions
module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)
module MatrixMap = Map.Make(String)

let translate (functions) =
  let context    = L.global_context () in
  let the_module = L.create_module context "MPL"

  (* ------------------------------------------------- *
   |             llvm tyoes declarations               |
   * ------------------------------------------------- *)
  and i32_t      = L.i32_type  context
  and i8_t       = L.i8_type   context
  and i1_t       = L.i1_type   context
  and void_t     = L.void_type context
  and float_t    = L.double_type context
  and array_t    = L.array_type
  and pointer_t  = L.pointer_type

  in
(*
  let matrix_int_t m n   = array_t (array_t i32_t m) n 
  and matrix_float_t m n = array_t (array_t float_t m) n 
  and matrix_t t m n   = array_t (array_t t m) n 
in
*)
 
  let rec func_ptr_t typ = 
      let arr  = Array.make 9 (ltype_of_typ typ) in
      let ftype = L.function_type (ltype_of_typ typ) arr in
      pointer_t ftype 
  and
       
  (* Find out the llvm type by Ast type *)
  ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.Void  -> void_t 
    | A.Mat(typ, rows, cols) ->
        (match typ with
                A.Int     -> array_t (array_t i32_t cols) rows
                | A.Float -> array_t (array_t float_t cols) rows
                | _ -> raise(Exceptions.UnsupportedMatrixType)        )
    | A.FMat(typ, rows, cols) -> array_t (array_t   (func_ptr_t typ) cols) rows
    | _ -> raise(Exceptions.UnsupportedType)
  in

  
  (* ------------------------------------------------- *
   |               Built-in Functions                  |
   * ------------------------------------------------- *)
  let printf_t            = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func         = L.declare_function "printf" printf_t the_module in

  let printm_int_t        = L.function_type i32_t [| L.pointer_type i8_t; i32_t; i32_t |] in
  let printm_int_func     = L.declare_function "printm_int" printm_int_t the_module in
  let printm_float_t      = L.function_type i32_t [| L.pointer_type i8_t; float_t; float_t |] in
  let printm_float_func   = L.declare_function "printm_float" printm_float_t the_module in

  let matrw_int_t         = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let matrw_float_t       = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let matread_int_func    = L.declare_function "matread_int" matrw_int_t the_module in
  let matread_float_func  = L.declare_function "matread_float" matrw_float_t the_module in
  let matwrite_int_func   = L.declare_function "matwrite_int" matrw_int_t the_module in
  let matwrite_float_func = L.declare_function "matwrite_float" matrw_float_t the_module in

  let memcpy_t            = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t|] in
  let memcpy_func         = L.declare_function "memcpy" memcpy_t the_module in

  let print_board_t       = L.function_type i32_t [| L.pointer_type i8_t; i32_t; i32_t; i32_t |] in
  let print_board_func    = L.declare_function "print_board" print_board_t the_module in

  let pgmread_func        = L.declare_function "pgmread" matrw_int_t the_module in
  let pgmwrite_func       = L.declare_function "pgmwrite" matrw_int_t the_module in
  let ppmrw_int_t         = L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t;
                            L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i32_t |] in
  let ppmread_func        = L.declare_function "ppmread" ppmrw_int_t the_module in
  let ppmwrite_func       = L.declare_function "ppmwrite" ppmrw_int_t the_module in

  (* Define each function so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name  = fdecl.S.sfname in 
      let lltyp = (ltype_of_typ fdecl.S.styp) in
      let arr   = Array.make 9 lltyp in
      let ftype = 
          if(name = "main") then 
              L.function_type lltyp [| lltyp |]
          else 
              L.function_type lltyp arr 
          in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.S.sfname function_decls in
    let builder           = L.builder_at_end context (L.entry_block the_function) in
    (* format strings for printf *)
    let int_format_str    = L.build_global_stringptr "%d\n" "fmti" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmts" builder in

    let get_mat_dimensions t = match t with
        A.Mat(typ, rows, cols) -> (typ, rows, cols)
        | _                    -> raise ( UnsupportedMatrixType ) 
    in

    (* An array of string representation of the 9 neighbhors *)
    let typ = fdecl.S.styp in
    let neighbor_names = [ "#NW"; "#N"; "#NE"; "#W"; "#C"; "#E"; "#SW"; "#S"; "#SE" ] in
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

	let add_local (m,mat_m) (t, n) =  
		let local_var = L.build_alloca (ltype_of_typ t) n builder in
		(match t with
			A.Mat(typ, row, cols) -> 
                let dim = get_mat_dimensions t in 
                ((StringMap.add n local_var m), (StringMap.add n dim mat_m)) 
			| _ -> ((StringMap.add n local_var m), mat_m)) 
    in

    let formals = 
        if(fdecl.S.sfname <> "main") then 
            (List.fold_left2 add_formal StringMap.empty neighbor_list
            (Array.to_list (L.params the_function))) 
        else StringMap.empty in
        
    (* Add the local variables to a new map *)
    List.fold_left add_local (formals, MatrixMap.empty) fdecl.S.slocals in

  (* ------------------------------------------------- *
   |               Helper Functions                    |
   * ------------------------------------------------- *)
    (* Return the value for a variable or formal argument *)
    let getSlocal (a, _) = a in
    let getMlocal (_, b) = b in
    let lookup n = try StringMap.find n (getSlocal local_vars) 
        with Not_found -> raise(Exceptions.LocalNotFound("unknown variable name: "^n)) 
    in 

    let find_matrix_type matrix =
      match (List.hd (List.hd matrix)) with
        S.SIntLit _   -> ltype_of_typ (A.Int)
      | S.SFloatLit _ -> ltype_of_typ (A.Float)
      | S.SBoolLit _  -> ltype_of_typ (A.Bool)
      | S.SId (s, t)  -> let func_type = L.type_of (fst (StringMap.find s function_decls))
		                    in pointer_t func_type 
      | _             -> raise (UnsupportedMatrixType) in 

    let idx n m          = [| L.const_int i32_t n; L.const_int i32_t m |] in
    let idx_gep      n m = [| L.const_int i32_t 0; L.const_int i32_t n; L.const_int i32_t m |] in

    let lookupM = function
        S.SId(s, t) -> MatrixMap.find s (getMlocal local_vars)
        | _ -> raise(Exceptions.UnsupportedMatrixType) 
    in

    let lookup_matrixid = function
        S.SId(s, t) -> lookup s 
        | _ -> raise(Exceptions.UnsupportedMatrixType) 
    in   

    let get_string_by_id = function
        | S.SId(s, t) -> s
        | _ -> raise(Exceptions.UnsupportedMatrixType)  (* TODO *)
    in

    let build_matrix ast_typ l expr builder =
        let typ           = ltype_of_typ ast_typ in
        let i32_list      = List.map (List.map (expr builder)) l in
        let list_of_arrs  = List.map Array.of_list i32_list in
        let arrs_of_arrs  = Array.of_list (List.map (L.const_array typ) list_of_arrs) in
        L.const_array (array_t typ (List.length (List.hd l))) arrs_of_arrs
    in

    let build_matrix_access i j s rows cols builder assign =
        let ptr = L.build_gep (lookup s) [| L.const_int i32_t 0; i; j|] s builder in
        if assign then ptr
        else L.build_load ptr s builder
    in

    let get_builder bb = L.builder_at_end context bb 
    in

    let get_neighbor mat i j xi xj row col b = 
        let rem x y = L.build_srem x y "tmp" b in
        let add x y = L.build_add  x y "tmp" b in
        let xi_l    = L.const_int i32_t xi in 
        let xj_l    = L.const_int i32_t xj in
        let x = if (xi == -1) then 
            rem (add (rem (add i xi_l) row) row) row
            else rem (add i xi_l) row
        in
        let y = if (xj == -1) then 
            rem (add (rem (add j xj_l) col) col) col
            else rem (add j xj_l) col
        in 
        L.build_load (L.build_gep mat [| L.const_int i32_t 0; x; y |] "build_gep"  b) "build_load" b
    in


    (* Build instructions for apply operation, this will translate a single 
     * apply to 9 distinct llvm function calls. 
     * @fname = string of the function name
     * @mat   = loaded llvalue that is a matrix *)
    let build_apply f_expr mat n b = 
	    let (typ, rows, cols) = (lookupM mat) in
        let dim = L.const_int i32_t (4 * rows * cols) in
        let mat_str = get_string_by_id mat in
        let f = get_string_by_id f_expr in
        let (fdef, fdecl) = StringMap.find f function_decls in
        let result = f ^ "_result" in
        
        (* Declare outter counter *) 
        let id_ptr      = L.build_in_bounds_gep (lookup mat_str) (idx 0 0) "build_in_bounds_gep" b in
        let mat_ptr     = L.build_bitcast id_ptr (pointer_t i8_t) "mat_ptr" b in
        let arr         = Array.make 9 (L.const_int i32_t 0) in
        (* let old_mat     = L.build_alloca (array_t (array_t i32_t cols) rows) "old_mat" b in *)
        let old_mat     = L.build_malloc (array_t (array_t i32_t cols) rows) "old_mat" b in 
        let old_mat_ptr = L.build_bitcast old_mat (pointer_t i8_t) "old_mat_ptr" b in
        let iptr        = L.build_alloca i32_t "outter_count" b in
        let jptr        = L.build_alloca i32_t "inner_count" b in
        ignore(L.build_store (L.const_int i32_t 0) iptr b);
        ignore(L.build_call memcpy_func [|  old_mat_ptr; mat_ptr; dim  |] "memcpy" b);

        let outter_pred_bb = L.append_block context "outter" the_function in
        ignore (L.build_br outter_pred_bb b);

        let outter_builder = L.builder_at_end context outter_pred_bb in
        let i = L.build_load iptr "outter_countv" outter_builder in
        let outter_bool_val = L.build_icmp L.Icmp.Slt i (L.const_int i32_t rows) "outter_bool_val" outter_builder in

        let outter_body_bb = L.append_block context "outter_body" the_function in

        (* Declare inner counter *) 
        let outter_body_builder = L.builder_at_end context outter_body_bb in
        ignore(L.build_store (L.const_int i32_t 0) jptr outter_body_builder);

          let inner_pred_bb = L.append_block context "inner" the_function in
          ignore (L.build_br inner_pred_bb outter_body_builder);
          let inner_builder = L.builder_at_end context inner_pred_bb in

          let j = L.build_load jptr "inner_countv" inner_builder in
          let inner_bool_val = L.build_icmp L.Icmp.Slt j (L.const_int i32_t cols) "inner_bool_val" inner_builder in

          let inner_body_bb = L.append_block context "inner_body" the_function in
          let inner_body_builder = L.builder_at_end context inner_body_bb in

          (* The actual code for function application *)
          let entry = L.build_gep (lookup mat_str) [| L.const_int i32_t 0; i; j |] mat_str inner_body_builder in

          (* for all the nine neighbors *)
          let arr = Array.make 9 (L.const_int i32_t 0) in 
          for n = -1 to 1 do
              for m = -1 to 1 do 
                  let index  = 3 * (m + 1) + (n + 1) in
                  arr.(index) <- get_neighbor old_mat i j m n 
                        (L.const_int i32_t rows) (L.const_int i32_t cols) inner_body_builder;
            done
          done;

          let res = L.build_call fdef arr result inner_body_builder in
          ignore(L.build_store res entry inner_body_builder);
          ignore(L.build_store (L.build_add j (L.const_int i32_t 1) "tmp" inner_body_builder) jptr inner_body_builder); (* j++ *)
          ignore(L.build_br inner_pred_bb inner_body_builder);

          let inner_merge_bb = L.append_block context "inner_merge" the_function in
          ignore(L.build_cond_br inner_bool_val inner_body_bb inner_merge_bb inner_builder);
          ignore(L.build_store (L.build_add i (L.const_int i32_t 1) "tmp" 
            (get_builder inner_merge_bb)) iptr (get_builder inner_merge_bb)); (* i++ *)
          ignore(L.build_br outter_pred_bb (get_builder inner_merge_bb));
        
          
        let outter_merge_bb = L.append_block context "outter_merge" the_function in
        ignore (L.build_cond_br outter_bool_val outter_body_bb outter_merge_bb outter_builder);
        let outter_merge_builder = get_builder outter_merge_bb in
        let ret = L.build_load (L.build_gep (lookup mat_str) [| L.const_int i32_t 0
            ; L.const_int i32_t 0; L.const_int i32_t 0 |] n outter_merge_builder) n outter_merge_builder in
        ignore(L.build_free old_mat outter_merge_builder);
        (ret, outter_merge_builder)
     in 

    let find_fptr_by_id typ builder = function
        S.SId (id, t) ->  L.build_bitcast (fst (StringMap.find id function_decls)) typ "func_ptr" builder 
        | _ -> raise(Exceptions.UnsupportedMatrixType)      
    in

    (* get an i8_t pointer by llvalue of a matrix, useful for C function calls *)
    let get_mptr m b = 
        let arr_ptr = L.build_in_bounds_gep m (idx 0 0) "build_in_bounds_gep" b in 
         L.build_bitcast arr_ptr (pointer_t i8_t) "mat_ptr" b 
    in

  (* ------------------------------------------------- *
   |               Expression Builder                  |
   * ------------------------------------------------- *)
    let rec expr builder expression =  match expression with
        S.SIntLit(i)                  -> L.const_int i32_t i
	  | S.SFloatLit(i)                -> L.const_float float_t i
      | S.SBoolLit b                  -> L.const_int i1_t (if b then 1 else 0)
      | S.SStrLit s                   -> L.build_global_stringptr s "str_lit" builder
      | S.SMatrixLit(l, Mat(t, r, c)) -> build_matrix t l expr builder
      (* | S.SNoexpr                  -> L.const_int i32_t 0 *)
      | S.SId (s, t)                  -> L.build_load (lookup s) s builder (* lookup s *)
      | S.SBinop (e1, op, e2, t) ->
          (match op with
            A.Add     -> L.build_add  (expr builder e1) (expr builder e2) "tmp" builder
          | A.Sub     -> L.build_sub  (expr builder e1) (expr builder e2) "tmp" builder
          | A.Mult    -> L.build_mul  (expr builder e1) (expr builder e2) "tmp" builder
          | A.Div     -> L.build_sdiv (expr builder e1) (expr builder e2) "tmp" builder
          | A.And     -> L.build_and  (expr builder e1) (expr builder e2) "tmp" builder
          | A.Or      -> L.build_or   (expr builder e1) (expr builder e2) "tmp" builder
          | A.Equal   -> L.build_icmp L.Icmp.Eq  (expr builder e1) (expr builder e2) "tmp" builder
          | A.Neq     -> L.build_icmp L.Icmp.Ne  (expr builder e1) (expr builder e2) "tmp" builder
          | A.Less    -> L.build_icmp L.Icmp.Slt (expr builder e1) (expr builder e2) "tmp" builder
          | A.Leq     -> L.build_icmp L.Icmp.Sle (expr builder e1) (expr builder e2) "tmp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt (expr builder e1) (expr builder e2) "tmp" builder
          | A.Geq     -> L.build_icmp L.Icmp.Sge (expr builder e1) (expr builder e2) "tmp" builder
          | A.Apply   -> fst (build_apply e1 e2 "tmp"  builder)
          | _         -> raise(Exceptions.InvalidUnaryOperation) 
          (* TODO: EMult, EDiv, Matapply *)
          ) 
      | S.SUnop(op, e, t) ->
          let e' = expr builder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) 
          e' "tmp" builder
      | S.SMatrixAccess (s, e1, e2, t) ->
            let (typ, rows, cols) = MatrixMap.find s (getMlocal local_vars) in
                (build_matrix_access (expr builder e1) (expr builder e2) s rows cols builder false)
      | S.SAssign (s, e, t) -> let e' = expr builder e in
                ignore (L.build_store e' (lookup s) builder); e'
      | S.SCall ("print", [e], t)  -> (match t with
          A.Int  -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
        | A.Bool -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
        | A.Mat(typ, rows, cols) -> let (typ, rows, cols) = (lookupM e) in
            let id = lookup_matrixid e 
            in (match typ with
            A.Int -> L.build_call printm_int_func [| (get_mptr id builder); (L.const_int i32_t rows); 
                (L.const_int i32_t cols) |] "printm_int" builder
            | A.Float -> L.build_call printm_float_func [| (get_mptr id builder); (L.const_int i32_t rows); 
                (L.const_int i32_t cols) |] "printm_float" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType) )
        | _      -> raise(Exceptions.IllegalArgument("from print")))
      | S.SCall ("prints", [e], t) -> 
	    L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
      | S.SCall ("print_board", [e1; e2], t) ->
		let (typ, rows, cols) = (lookupM e1) in
        let id = lookup_matrixid e1 in
        (match typ with
            A.Int -> L.build_call print_board_func [| (get_mptr id builder) ; (L.const_int i32_t rows); 
                (L.const_int i32_t cols); (expr builder e2) |] "print_board" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType))
      | S.SCall ("printm", [e], t) ->
		let (typ, rows, cols) = (lookupM e) in
        let id = lookup_matrixid e 
                in (match typ with
                            A.Int -> L.build_call printm_int_func [| (get_mptr id builder); (L.const_int i32_t rows); 
                                (L.const_int i32_t cols) |] "printm_int" builder
                            | A.Float -> L.build_call printm_float_func [| (get_mptr id builder); (L.const_int i32_t rows); 
                                (L.const_int i32_t cols) |] "printm_float" builder
                            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | S.SCall ("matwrite", [e1; e2], t) ->
		let (typ, rows, cols) = (lookupM e2) in
        let id = lookup_matrixid e2 
        in (match typ with
            A.Int -> L.build_call matwrite_int_func [| (expr builder e1); (get_mptr id builder);
                (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matwrite_int" builder
            | A.Float -> L.build_call matwrite_float_func [| (expr builder e1); (get_mptr id builder);
                (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matwrite_float" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | S.SCall ("matread", [e1; e2], t)  ->
		let (typ, rows, cols) = (lookupM e2) in
        let id = lookup_matrixid e2
        in (match typ with
            A.Int -> L.build_call matread_int_func [| (expr builder e1); (get_mptr id builder);
                (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matread_int" builder
            | A.Float -> L.build_call matread_float_func [| (expr builder e1); (get_mptr id builder);
                (L.const_int i32_t rows); (L.const_int i32_t cols) |] "matread_float" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | S.SCall ("ppmwrite", [e1; e2; e3; e4], t) ->
		let (typ, rows, cols) = (lookupM e2) in
        let mat1 = lookup_matrixid e2 in
        let mat2 = lookup_matrixid e3 in
        let mat3 = lookup_matrixid e4 
        in (match typ with
            A.Int -> L.build_call ppmwrite_func [| (expr builder e1); (get_mptr mat1 builder);
                (get_mptr mat2 builder); (get_mptr mat3 builder); (L.const_int i32_t rows); 
                (L.const_int i32_t cols) |] "ppmwrite" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | S.SCall ("ppmread", [e1; e2; e3; e4], t) ->
		let (typ, rows, cols) = (lookupM e2) in
        let mat1 = lookup_matrixid e2 in
        let mat2 = lookup_matrixid e3 in
        let mat3 = lookup_matrixid e4 
        in (match typ with
            A.Int -> L.build_call ppmread_func [| (expr builder e1); (get_mptr mat1 builder);
                (get_mptr mat2 builder); (get_mptr mat3 builder); (L.const_int i32_t rows); 
                (L.const_int i32_t cols) |] "ppmread" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | S.SCall ("pgmwrite", [e1; e2], t)  ->
		let (typ, rows, cols) = (lookupM e2) in
        let id = lookup_matrixid e2
        in (match typ with
            A.Int -> L.build_call pgmwrite_func [| (expr builder e1); (get_mptr id builder);
                (L.const_int i32_t rows); (L.const_int i32_t cols) |] "pgmwrite" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | S.SCall ("pgmread", [e1; e2], t)  ->
		let (typ, rows, cols) = (lookupM e2) in
        let id = lookup_matrixid e2
        in (match typ with
            A.Int -> L.build_call pgmread_func [| (expr builder e1); (get_mptr id builder);
                (L.const_int i32_t rows); (L.const_int i32_t cols) |] "pgmread" builder
            | _ -> raise(Exceptions.UnsupportedMatrixType)        )
      | _ -> raise(Exceptions.StatementNotSuuported)
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in

  (* ------------------------------------------------- *
   |               Statement Builder                   |
   * ------------------------------------------------- *)
    let rec stmt builder = function
        S.SBlock sl -> List.fold_left stmt builder sl
      | S.SExpr e -> (match e with
          S.SBinop(e1, op, e2, t) -> (match op with
              A.Apply -> snd (build_apply e1 e2 "tmp"  builder)
              |_ -> ignore (expr builder e); builder)
          | _ -> ignore (expr builder e); builder)
      | S.SReturn e -> ignore (match fdecl.S.styp with
	        A.Void -> L.build_ret_void builder
	        | _ -> L.build_ret (expr builder e) builder); builder
      | S.SIf (predicate, then_stmt, else_stmt) ->
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

      | S.SWhile (predicate, body) ->
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
      (*| A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
      *)
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (S.SBlock fdecl.S.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.S.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
