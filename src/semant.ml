(*
 * File: sement.ml
 * Date: 2017-03-28
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

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
let requireIntegers tlist str = 
    let _ = List.map(
          fun t ->  match t with 
            IntLit(_) -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let requireFloats tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            FloatLit(_) -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let requireBools tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            BoolLit(_) -> true
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
    let i = List.length d2list in
    let j = List.length (List.hd d2list) in
    let t = List.hd (List.hd d2list) in
        match t with
            IntLit(_) -> List.map (fun lst -> requireIntegers lst str) d2list; Mat(Int, i, j)
          | FloatLit(_) -> List.map (fun lst -> requireFloats lst str) d2list; Mat(Float, i, j)
          | BoolLit(_) -> List.map (fun lst -> requireBools lst str) d2list; Mat(Bool, i, j)
          | _ -> raise (Failure("Matrix literals must be of the same type"))

let rec checkUnique lst = 
  if (List.length lst)==1 then true else ((List.hd lst) ==(List.nth lst 1) && (checkUnique(List.tl lst)))

let checkMatrixDimensions d2list str =
    if ((checkUnique (List.map List.length d2list))==true) then true else raise(Failure(str))

let getArithBinopType t1 t2 op =
  match(t1, t2) with
  (Int, Int) -> Int
  | (Float, Float) -> Float
  | (Mat(typ1, i1, j1), Mat(typ2, i2, j2)) ->
    (match op with
      Add | Sub -> if typ1=typ2 && i1=j1 && j1=j2 then Mat(typ1, i1, j1)
            else raise(Failure("Matrices must be of same type and dimensions for +/-"))
      | Mult -> if typ1=typ2 && i2=j1 then Mat(typ1, i1, j2)
            else raise(Failure("M1(a,b) and M2(c,d) must have b=c for *"))
      | _ -> raise(Failure("No matrices division")))
  | _ -> raise(Failure("Invalid type for arithmetic operand"))

(* let getLogicalBinopType t1 t2 op = function *)
let getLogicalBinopType t1 t2 op = 
  match (t1, t2) with 
  (Int, Int) -> Bool
  | (Float, Float) -> Bool
  | _ -> raise(Failure("Invalid type for logical operand"))

let getEqualityBinopType t1 t2 op =
  match (t1, t2) with 
    (Int, Int) -> Bool
  | (Float, Float) -> Bool
  | _ -> raise(Failure("Invalid type for logical operand"))

(*fd is where you feed function_decls*)
let checkFunction fd s = try StringMap.find s fd 
       with Not_found -> raise (Failure ("unrecognized function " ^ s))

let checkApply t1 t2 op fd = 
    let func = checkFunction fd t1 in
        match t2 with
        Mat(typ,_,_) -> if func.typ==typ then t2 else raise(Failure("Function and Matrix Type don't match for apply"))
        | _ -> raise(Failure("T2 must be a matrix type"))

let checkBinop op t1 t2 fd=
  begin match op with
  Add | Mult | Sub | Div -> getArithBinopType t1 t2 op
  | Equal | Neq -> getEqualityBinopType t1 t2 op
  | And | Or -> getLogicalBinopType t1 t2 op
  | Less | Leq | Greater | Geq -> getEqualityBinopType t1 t2 op
  | _ -> raise(Failure("Invalid operand in getBinopType"))
  end


let check (functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
  n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err = 
  match (lvaluet,rvaluet) with
      Mat(t1, i1, j1), Mat(t2, i2, j2) -> if t1=t2 && i1=i2 && j1=j2 then lvaluet else raise err
     | _-> if lvaluet == rvaluet then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)
(*
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
*)
  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] }))
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =
(*
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);
*)
    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
  StringMap.empty (func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
       IntLit _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | MatrixLit s -> checkMatrixDimensions s "Malformed matrix"; checkAllMatrixLiterals s "All matrix literals must be of the same type"; 
      | Binop(e1, op, e2) as e -> 
             (match (e1,op) with
             (Id s,Apply) -> checkApply s (expr e2) op function_decls
            | _ -> let t1 = expr e1 and t2 = expr e2 in 
          checkBinop op t1 t2 function_decls)
      | Unop(op, e) as ex -> let t = expr e in
   (match op with
     Neg when t = Int -> Int
   | Neg when t = Float -> Float
   | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator")))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    in 
  
    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression"))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
  Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives invalid type"))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      (*| For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st*)
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions

