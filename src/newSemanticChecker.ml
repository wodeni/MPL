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

type symbol_table = {
    parent : symbol_table option;
    variables : bind list;
}

type translation_environment = {
    scope : symbol_table;
    (* return_type: variable_type; *)
    (*current_func: func_decl option;*)
    (*functions : func_decl list;*)
    (* exception_scope: exception_scope; *)
}

module StringMap = Map.Make(String)

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

let requireBools tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            Bool -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let requireAllMatrices tlist str =
    let _ = List.map(
        fun t -> match t with
            Mat(typ1, i1, j1) -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true


let checkAllMatrixLiterals d2list str =
    let t = List.hd (List.hd d2list) in
        let _ = match t with
            Int -> List.map (fun lst -> requireIntegers lst str) d2list
          | Float -> List.map (fun lst -> requireFloats lst str) d2list
          | Bool -> List.map (fun lst -> requireBools lst str) d2list
          | _ -> raise (Failure("First matrix literal is weird"))
        in
        t

(* let checkMatrixDimensions d2list str =
    if (List.length (checkUnique (List.map List.length d2list))==1) then true else raise(Failure(str))

let checkUnique lst = 
  if (List.length lst)==1 then true else ((List.hd lst) ==(List.nth lst 1) && (checkUnique(List.tl lst))) *)


let checkNums tlist = match List.hd tlist with
  Int -> requireIntegers tlist "All of tlist must be Integers" 
  | Float -> requireFloats tlist "All of tlist must be Floats"
  | _ -> raise(Failure ("Invalid first type in checkNums"))

(* find functions *)
(* let rec find_variable (scope : symbol_table) name = 
    try 
        (* do match with the different types of variables in the List.find
         * function *)
        List.find ( fun var_decl ->
            begin match var_decl with 
            Array_decl(_, _, s) -> s = name
            | Var(_, s) -> s = name 
            | VarInit(_, s, _) -> s = name
            end ) scope.variables
    with Not_found ->
        match scope.parent with 
          Some(parent) -> find_variable parent name
          | _ -> raise Not_found *)

let getArithBinopType t1 t2 op = function
  (Ast.Int, Ast.Int) -> Ast.Int
  | (Ast.Float, Ast.Float) -> Ast.Float
  | (Ast.Mat(typ1, i1, j1), Ast.Mat(typ2, i2, j2)) ->
    (match op with
      Add | Sub -> if typ1=typ2 && i1=j1 && j1=j2 then Ast.Mat(typ1, i1, j1)
            else raise(Failure("Matrices must be of same type and dimensions for +/-"))
      | Mult -> if typ1=typ2 && i2=j1 then Ast.Mat(typ1, i1, j2)
            else raise(Failure("M1(a,b) and M2(c,d) must have b=c for *"))
      | _ -> raise(Failure("No matrices division")))
  | _ -> raise(Failure("Invalid type for arithmetic operand"))

let getLogicalBinopType t1 t2 op = function
  (Ast.Int, Ast.Int) -> Ast.Bool
  | (Ast.Float, Ast.Float) -> Ast.Bool
  | _ -> raise(Failure("Invalid type for logical operand"))

let getEqualityBinopType t1 t2 op = function
    (Ast.Int, Ast.Int) -> Ast.Bool
  | (Ast.Float, Ast.Float) -> Ast.Bool
  | _ -> raise(Failure("Invalid type for logical operand"))

let checkBinop t1 t2 op =
  match op with
  Add | Mult | Sub | Div -> getArithBinopType t1 t2 op
  | Equal | Neq -> getEqualityBinopType t1 t2 op
  | And | Or -> getLogicalBinopType t1 t2 op
  | Less | Leq | Greater | Geq -> getEqualityBinopType t1 t2 op
  | _ -> raise(Failure("Invalid operand in getBinopType"))

let rec checkExpr env = function
  Ast.IntLit(v) -> (Ast.IntLit(v), Ast.Int)
  | Ast.FloatLit(v) -> (Ast.FloatLit(v), Ast.Float)
(*     | Ast.StrLit(v) -> (Ast.StrLit(v), Ast.String) *)
    | Ast.BoolLit(v) -> (Ast.BoolLit(v), Ast.Bool)
(*     | Ast.Id(vname) -> 
                let vdecl = (try
                find_variable env.scope vname 
                with Not_found -> 
                    raise(Failure("Invalid variable usage " ^ vname))) in
                let (_, t) = vdecl in
                (Ast.Id(vname), t) *)
    | Ast.Binop(e1, op, e2) ->
                let (e1, t1) = checkExpr env e1
                and (e2, t2) = checkExpr env e2 in
                (Ast.Binop(e1, op, e2), (checkBinop t1 t2 op))
    | Ast.MatrixLit(d2list) -> 
                let (* _ = checkMatrixDimensions "Malformed matrix" *)
                t = checkAllMatrixLiterals "All entries must be of the same type"
                  in (Ast.MatrixLit(d2list), t)
        (* assign, matrix access, call, matrix literal *)

(*This returns the type of e and raises a flag if inconsistent or invalid syntax*)
 let checkType e = function
   IntLit _ -> Int
  |FloatLit _ -> Float
  |BoolLit _ -> Bool
  |MatrixLit m -> checkMatrixLiterals m   (*To be done later*)
  |Id s -> getIDType s

