(*
 * File: codegen.mll
 * Date: 2017-03-13
 *
 * PLT Spring 2017
 * MPL Project
 * Wode "Nimo" Ni    <wn2155@columbia.edu>
 * David Rincon-Cruz <dr2884@columbia.edu>
 * Chi Zhang         <cz2440@columbia.edu>
 * Jiangfeng Wang    <jw3107@columbia.edu>
 *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

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

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    (*
    | A.Matrix(typ, rows, cols) ->
        let rows' = match rows with Int_lit(s) -> s | _ -> raise(Exceptions.InvalidMatrixDimension) in
        let cols' = match cols with Int_lit(s) -> s | _ -> raise(Exceptions.InvalidMatrixDimension) in
        (match typ with
                A.Int      -> array_t (array_t i32_t cols') rows'
    *)
    | _ -> raise(Exceptions.UnsupportedMatrixType))
  in

  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* TODO: Declare the built-in printbig() function *)
  (* TODO: We could define printm() function here *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmti" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmts" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

    (* NOTE: We do not have any argument. Might not need this
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in
    *)

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found ->  raise (Error "unknown variable name")
    in


    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Litstr s -> L.build_global_stringstr s ("str_" ^ s) builder
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
          (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          (* TODO: EMult, EDiv, Apply, Matapply *)
          ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
          let e' = expr builder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) 
          e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	    L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | A.Call ("prints", [e]) -> 
	    L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
      | A.Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      (* TODO: to be implemented
      | A.Call ("printm", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printm" builder
      *)
      (* NOTE: we do not have any user defined functions
       *       Will use this code once we implement "@"
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    *)
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

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
