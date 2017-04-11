(*
* File: sement.ml <---- I don't know we're rewriting from scratch
* Date: 2017-04-10
*
* PLT Spring 2017
* MPL Project
* Wode "Nimo" Ni    <wn2155@columbia.edu>
* David Rincon-Cruz <dr2884@columbia.edu>
* Chi Zhang         <cz2440@columbia.edu>
* Jiangfeng Wang    <jw3107@columbia.edu>
*)

open Ast
 
module StringMap = Map.Make(String)

let getIDType s =
	try StringMap.find s symbols
	with Not_found -> raise (Failure ("undeclared identifier " ^ s))

let requireIntegers tlist str = 
    let _ = List.map(
        	fun t ->  match t with 
            Int -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let requireFloats tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            Float -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true


let checkNums tlist = match List.hd tlist with
	Int -> requireIntegers tlist "All of tlist must be Integers"
	| Float -> requireFloats tlist "All of tlist must be Floats"
	| Mat(typ1, i1, j1) 
	|_ -> raise(Failure ("Invalid first type in checkNums"))

let getArithSBinop se1 se2 op = function
	(Int, Int) -> SBinop(se1, op, se2, Int)
	| (Float, Float) -> SBinop(se1, op, se2, Float)
	| (Matrix(typ1, i1, j1), Matrix(typ2, i2, j2)) ->
			(match op with
				Add | Sub 	->
					if typ1=typ2 && i1=i2 && j1=j2 then
						SBinop(se1, op, se2, Mat(typ1, i1, j2))
					else raise(Failuer("Matrices must be same type and dimensions for +/-"))
				| Mult 		->
					if typ1=typ2 && j1 = i2 then
						SBinop(se1, op, se2, Mat(typ1, i1, j2))
					else raise(Failuer("Matrices M1(i1,j1) and M2(i2,j2) must have j1 = i2 and be of same type to be multiplied"))
				| _ -> raise(Failuer("Cannot divide matrices"))

let checkArithBinop e1 op e2 =
	let se1 = exprToSexpr e1 in
	let se2 = exprToSexpr e2 in
	let t1 = sexprToExpr se1 in
	let t2 = sexprToExpr se2 in
	match op with
		Add|Sub|Mult|Div -> getArithSBinop se1 se2 op (t1, t2) (*(let _ = checkNums[e1;e2] in e1)*)
		| Equal | Neq -> when t1 = t2 -> SBinop(se1, op, se2, Bool)
		| And | Or -> when t1 = Bool && t2 = Bool -> SBinop(se1, op, se2, Bool)
		| Less | Leq | Greater | Geq when t1 = t2 && (t1 = Int) || t1 = Float) -> SBinop(se1, op, se2, t1)
		|_ -> raise (Failure ("Invalid operand in checkArithBinop"))

(*This returns the type of e and raises a flag if inconsistent or invalid syntax*)
 let checkType e = function
	 IntLit _ -> Int
	|FloatLit _ -> Float
	|BoolLit _ -> Bool
	|MatrixLit m -> checkMatrixLiterals m 	(*To be done later*)
	|Id s -> getIDType s

let rec getIDType s =
	try StringMap.find s
	with | Not_found -> raise (Failure("Undefined ID type"))

let exprToSExpr expr = function
	  IntLit(n) 				-> SIntLit(n)
	| FloatLit(n)				-> SFloatLit(n)
	| BoolLit(b)       			-> SBoolLit(b)
	| StringLit(s)        		-> SStringLit(s)
	| Id(s)                		-> SId(s, getIDType s)
	| Null                 		-> SNull
	| Noexpr               		-> SNoexpr
	| Unop(op, e)          		-> checkUnop fname_map func_st op e
	| Assign(s, e)   			-> checkAssign fname_map func_st s e
	| Binop(e1, op, e2)    		-> checkArithBinop e1 op e2
	| Call(s, el)				-> let fd = function_decl s fname_map in
		if List.length el != List.length fd.formals then
			raise (Exceptions.IncorrectNumberOfArguments(fd.fname, List.length el, List.length fd.formals))
		else
		SCall(s, List.map (expr_to_sexpr fname_map func_st) el, fd.return_type)
	| MatrixAccess(s, e1, e2)	-> check_matrix_access fname_map func_st s e1 e2
	| MatrixLit(nll)			-> check_matrix_lit fname_map func_st nll

and sexprToExpr sexpr = match sexpr with
	  SIntLit(_)						-> Int
	| SFloatLit(_)						-> Float
	| SBoolLit(_)						-> Bool
	| SStringLit(_) 					-> String
	| SNoexpr 							-> Void
	| SNull								-> Null
	| SId(_, t) 						-> t
	| SBinop(_, _, _, t) 				-> t
	| SAssign(_, _, t) 					-> t
	| SCall(_, _, t)					-> t
	| SUnop(_, _, t) 					-> t
	| SMatrixAccess(_, _, _, t)			-> t
	| SMatrixLit(l, t)				->
		let c = List.length (List.hd l) in
		let r = List.length l in
		(match d with
			Int 				-> Matrix(Int, IntLit(r), IntLit(c))
			| Float				-> Mat(Float, IntLit(r), IntLit(c))
			| _ 				-> raise(Failure("Unsupported matrix type"))
