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
open Sast
(*module StringMap = Map.Make(String)*)
module FuncMap = Map.Make(String)

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
  | (Int, Mat(typ, i, j)) -> (match op with 
                                Mult -> Mat(typ, i, j)
                                | _ -> raise(Failure("Only valid operation between int and matrix is multiplication.")))
  | (Float, Mat(typ, i, j)) -> (match op with 
                                Mult -> Mat(typ, i, j)
                                | _ -> raise(Failure("Only valid operation between float and matrix is multiplication.")))
  | (Mat(typ, i, j),Int) -> (match op with 
                                Mult -> Mat(typ, i, j)
                                | _ -> raise(Failure("Only valid operation between int and matrix is multiplication.")))
  | (Mat(typ, i, j),Float) -> (match op with 
                                Mult -> Mat(typ, i, j)
                                | _ -> raise(Failure("Only valid operation between float and matrix is multiplication.")))
  | (Mat(typ1, i1, j1), Mat(typ2, i2, j2)) ->
    (match op with
      Add | Sub -> if typ1=typ2 && i1=i2 && j1=j2 then Mat(typ1, i1, j1)
            else raise(Failure("Matrices must be of same type and dimensions for +/-"))
      | Mult -> if typ1=typ2 && j1=i2 then Mat(typ1, i1, j2)
            else raise(Failure("M1(a,b) and M2(c,d) must have b=c for *"))
      | Emult|Ediv -> if typ1=typ2 && i1=i2 && j1=j2 then Mat(typ1, i1, j2)
            else raise(Failure("M1(a,b) and M2(c,d) must have matching dimensions .* and ./"))      
      | _ -> raise(Failure("No matrices division")))
  | _ -> raise(Failure("Invalid type for arithmetic operand"))

(* let getLogicalBinopType t1 t2 op = function *)
let getLogicalBinopType t1 t2 op = 
  match (t1, t2) with 
  (Int, Int) -> Bool
  | (Float, Float) -> Bool
  | (Bool, Bool) -> Bool
  | _ -> raise(Failure("Invalid type for logical operand"))

let getEqualityBinopType t1 t2 op =
  match (t1, t2) with 
    (Int, Int) -> Bool
  | (Float, Float) -> Bool
  | _ -> raise(Failure("Invalid type for logical operand"))


(*fd is where you feed function_decls*)
let checkFunction fd s = try FuncMap.find s fd 
       with Not_found -> raise (Failure ("unrecognized function " ^ s))

let checkApply t1 t2 op fd = 
    let func = checkFunction fd t1 in
         let t21 = Sast.get_expr_type_info t2 in
        match t21 with
        Mat(typ,_,_) -> if func.typ==typ then t21 else raise(Failure("Function and Matrix Type don't match for apply"))
        | _ -> raise(Failure("T2 must be a matrix type"))

(*Checks that a list of strings are functions with a specific type*)
let requireFunctionsWithType tlist fd typ = 
    let _ = List.map (fun func -> if func.typ==typ then () else raise(Failure(""^func.fname^" has wrong type: "^(string_of_typ typ)))) (List.map (fun s -> checkFunction fd s) tlist)
    in true

(*Checks that a Fmatrix of functions is homogeneous*)
let checkFMatFunctions d2list fd =
    let i = List.length d2list in
    let j = List.length (List.hd d2list) in
    let func = checkFunction fd (List.hd (List.hd d2list)) in
    let typ = func.typ in
    let _ = List.map (fun lst -> requireFunctionsWithType lst fd typ) d2list in FMat(typ,i,j)


let checkBinop op e1 e2 fd=
  let t1 = Sast.get_expr_type_info e1 and t2 = Sast.get_expr_type_info e2 in
  match op with
  Add | Mult | Sub | Div -> getArithBinopType t1 t2 op
  | Equal | Neq -> getEqualityBinopType t1 t2 op
  | And | Or -> getLogicalBinopType t1 t2 op
  | Less | Leq | Greater | Geq -> getEqualityBinopType t1 t2 op
  | _ -> raise(Failure("Invalid operand in getBinopType"))


let checkMatIndex m i j = match(m,i,j) with
  (Mat(typ,row,col), IntLit(i1), IntLit(j1)) -> if (i1<0)||(i1>=row)
                                    then raise(Failure("Out of bounds access - row:"^(string_of_int row)^" i:"^(string_of_expr i))) 
                                    else if (j1<0)||(j1>=col) 
                                        then raise(Failure("Out of bounds access - col:"^(string_of_int col)^" j:"^(string_of_expr j))) 
                                        else typ
  |(Mat(typ,row,col), Id(_), Id(_)) -> typ 
  |(Mat(typ,row,col), IntLit(i1), Id(_)) -> if (i1<0)||(i1>=row)
                                    then raise(Failure("Out of bounds access - row:"^(string_of_int row)^" i:"^(string_of_expr i))) 
                                    else typ
  |(Mat(typ,row,col), Id(_), IntLit(j1)) -> if (j1<0)||(j1>=col) 
                                            then raise(Failure("Out of bounds access - col:"^(string_of_int col)^" j:"^(string_of_expr j))) 
                                            else typ
  | _-> raise(Failure("Invalid arguments in accessing matrix"))

let numlitToSlit n = match n with
    IntLit(n) -> SIntLit(n)
  | FloatLit(n) -> SFloatLit(n)
let getLitType n = match n with
    IntLit(_) -> Int
  | FloatLit(_) -> Float
  | _ -> raise(Failure("Must be int or float type"))

let getString s = match s with
    SId(n, _) -> n
  | _ -> raise(Failure("Cannot find symbol"))

let mlitToSmlit n = 
    let t = getLitType(List.hd (List.hd n)) in
    let slit = (List.map (fun nl -> (List.map numlitToSlit nl)) n) in
    let r = List.length n and c = List.length (List.hd n) in
    SMatrixLit(slit, Mat(t,r,c))
(*
let get_expr_type_info sexpr = match sexpr with
    |SIntLit _ ->                Int
    |SFloatLit _ ->              Float
    |SBoolLit _ ->               Bool
    |SMatrixLit (_,x) ->         x
    |SId (_,x) ->                x
    |SStrLit _ ->                String
    |SBinop (_,_,_,x) ->         x
    |SUnop (_,_,x) ->            x
    |SAssign (_,_,x) ->          x
    |SCall (_,_,x) ->            x
    |SNull ->                    Void
    |SMatrixAccess (_,_,_,x) ->  x

let exprToSexpr expr = match expr with
    | IntLit(n)  ->              SIntLit(IntLit(n))
    | FloatLit(n) ->             SFloatLit(FloatLit(n))
    | BoolLit(n) ->              SBoolLit(BoolLit(n))
    | MatrixLit(n) ->            checkMatrixLit n
    | Id(n) ->                   SId(n, getIdType n)
    | StrLit(n) ->               StrLit(n)
    | Binop(e1, op, e2) ->       checkBinop e1 op e2
    | Unop(op, e) ->             checkUnop op e
    | Assign(s, e) ->            checkAssign s e
    | Call(s, e) ->              SCall(s, (List.map exprToSexpr e), )
    | Null ->                    SNull
    | MatrixAccess(s, e1, e2) -> checkMatrixAccess s e1 e2
*)
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
  let built_in_decls =  FuncMap.add "print"
     { typ = Void; fname = "print"; formals = [];
       locals = []; body = [] } 
     (FuncMap.add "printb" { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } 
     (FuncMap.add "prints" { typ = Void; fname = "prints"; formals = [(String, "x")];
       locals = []; body = [] } 
     (FuncMap.add "printm" { typ = Void; fname = "printm"; formals = [];
       locals = []; body = [] } 
     (FuncMap.add "matread"{ typ = Void; fname = "matread"; formals = [];
        locals = []; body = []} 
     (FuncMap.add "pgmread"{ typ = Void; fname = "pgmread"; formals = [];
        locals = []; body = []} 
     (FuncMap.add "ppmread"{ typ = Void; fname = "pgmread"; formals = [];
        locals = []; body = []} 
     (FuncMap.add "matwrite"{ typ = Void; fname = "matwrite"; formals = [];
        locals = []; body = []} 
     (FuncMap.add "pgmwrite"{ typ = Void; fname = "pgmwrite"; formals = [];
        locals = []; body = []} 
     (FuncMap.add "ppmwrite"{ typ = Void; fname = "pgmwrite"; formals = [];
        locals = []; body = []} 
     (FuncMap.singleton "print_board"{ typ = Void; fname = "print_board"; formals = [];
        locals = []; body = []} 
     ))))))))))
   in
     
  let function_decls = List.fold_left (fun m fd -> FuncMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try FuncMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

let check_function func =
    let module StringMap = Map.Make(String) in
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
    
    let neighbor_names = [ "#NW"; "#N"; "#NE"; "#W"; "#C"; "#E"; "#SW"; "#S"; "#SE" ] 
    in

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m s -> StringMap.add s func.typ m) (List.fold_left (fun m (t, n) -> StringMap.add n t m)
  StringMap.empty (func.locals )) neighbor_names
    in

    let initSyms = ref( List.fold_left (fun m (t, n) -> StringMap.add n false m) StringMap.empty (func.locals))
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    
    let updateInitSyms var = 
        initSyms := StringMap.add var true !initSyms
    in
    let checkInitSyms n = 
        let i = StringMap.find n !initSyms in
        if i == false then raise(Failure("The variable " ^ n ^ " has not been initialized"))
        else true
     in
    
    (* Return the type of an expression or throw an exception *)
    let rec expr = function
       IntLit(n) -> SIntLit(n)
      | FloatLit(n)  -> SFloatLit(n)
      | BoolLit(n) -> SBoolLit(n)
      | StrLit(n) -> SStrLit(n)
      | Id(s) -> SId(s, type_of_identifier s)
      | MatrixLit s ->  ((checkMatrixDimensions s "Malformed matrix"; checkAllMatrixLiterals s "All matrix literals must be of the same type")); (mlitToSmlit s)
      | MatrixAccess(m,i,j) -> let t = checkMatIndex (type_of_identifier m) i j in
            SMatrixAccess(m, numlitToSlit i, numlitToSlit j, t)
      | Binop(e1, op, e2) as e -> 
             (match (e1,op) with
             (Id s,Apply) -> SBinop(SId(s,get_expr_type_info (expr e2)), op, (expr e2), checkApply s (expr e2) op function_decls)
            | _ -> let t1 = expr e1 and t2 = expr e2 in 
          SBinop(t1, op, t2, checkBinop op t1 t2 function_decls))
      | Unop(op, e) as ex -> SUnop(op, (expr e), (let t1 = expr e in
                                                  let t = Sast.get_expr_type_info t1 in
   (match op with
     Neg when t = Int -> Int
   | Neg when t = Float -> Float
   | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator"))) ))
      (*| Noexpr -> Void*)
      | Assign(var, e) as ex -> SAssign(var, (expr e), (let lt = type_of_identifier var
                                and rt1 = (expr e) in 
                                let  rt = (Sast.get_expr_type_info rt1) in
                                let  ret = check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt )) in
                                let _ = updateInitSyms var in
                                ret ))
      | Call("printm", actuals) -> let a = (List.map expr actuals) in
                        let s = getString (List.hd a) in
                        SCall("printm", a, (if (List.length actuals != 1)
                                           then raise(Failure("Too many arguments to printm"))
                                           else let t1 = expr (List.hd actuals) in
                                           let _ = checkInitSyms s in
                                           let t = Sast.get_expr_type_info t1 in
                                           (match t with
                                           |Mat(Int,_,_) -> Void
                                           |Mat(Float,_,_) -> Void
                                           |_ -> raise(Failure("Wrong argument type. [Not mat<int> or mat<float>]")))
                                    )) (*match scall*)
      | Call("print", actuals) -> let a = (List.map expr actuals) in
                                        if (List.length actuals != 1)
                                           then raise(Failure("Too many arguments to print"))
                                           else (
                                           let l = List.hd a in
                                           (match l with
                                           SIntLit(_) -> SCall("print", a, Int)
                                           | SFloatLit(_) ->  SCall("print", a, Float)
                                           | SBoolLit(_) -> SCall("print",a,Bool)
                                           | SBinop(_,_,_,x) -> SCall("print", a, x)
                                           | _ -> SCall("print", a,
                                           (let t1 = expr (List.hd actuals) in
                                           let s = getString (List.hd a) in
                                           let _ = checkInitSyms s in
                                           let t = Sast.get_expr_type_info t1 in
                                           (match t with
                                           |Int -> Int
                                           |Float -> Float
                                           |Bool -> Bool
                                           |_ -> raise(Failure("Wrong argument type. [int or float or bool]")))
                                    )))) (*match scall*)
      | Call("prints", actuals) -> let a = (List.map expr actuals) in
                                         
                                        if (List.length actuals != 1)
                                           then raise(Failure("Too many arguments to prints"))
                                           else( 
                                              let str = List.hd actuals in
                                              (match str with
                                              StrLit(_)->SCall("prints", a, String)
                                            | _ -> (SCall("prints", a,
                                                (let t1 = expr (List.hd actuals) in
                                                let s = getString (List.hd a) in
                                                let _ = checkInitSyms s in
                                                let t = Sast.get_expr_type_info t1 in
                                                    (match t with
                                                    |String -> String
                                                    |_ -> raise(Failure("Wrong argument type. [String]")))
                                    ))))) (*match scall*)
      | Call("matread", actuals) -> let a = (List.map expr actuals) in 
              SCall("matread", a,    (if (List.length actuals != 2) 
                                    then raise(Failure("matread only accepts 2 arguments"))
                                    else ( let a1 = List.hd actuals and a2 = expr (List.nth actuals 1) in
                                    let t2 = Sast.get_expr_type_info a2 in
                                    let var = getString a2 in
                                        match (a1,t2) with
                                        StrLit(_),Mat(_,_,_) -> updateInitSyms var; Int
                                        | _ -> raise(Failure("matread takes string literal and matrix type")) 
                                     ) ))
     | Call("matwrite", actuals) -> let a = (List.map expr actuals) in 
              SCall("matwrite", a,    (if (List.length actuals != 2) 
                                    then raise(Failure("matwrite only accepts 2 arguments"))
                                    else ( let a1 = List.hd actuals and a2 = expr (List.nth actuals 1) in
                                    let t2 = Sast.get_expr_type_info a2 in
                                    let var = getString a2 in
                                        match (a1,t2) with
                                        StrLit(_),Mat(_,_,_) -> updateInitSyms var; Void
                                        | _ -> raise(Failure("matwrite takes string literal and matrix type")) 
                                     ) ))
      | Call("pgmread", actuals) -> let a = (List.map expr actuals) in 
              SCall("pgmread", a,    (if (List.length actuals != 2) 
                                    then raise(Failure("pgmread only accepts 2 arguments"))
                                    else ( let a1 = List.hd actuals and a2 = expr (List.nth actuals 1) in
                                    let t2 = Sast.get_expr_type_info a2 in
                                    let var = getString a2 in
                                        match (a1,t2) with
                                        StrLit(_),Mat(_,_,_) -> updateInitSyms var; Int
                                        | _ -> raise(Failure("pgmread takes string literal and matrix type")) 
                                     ) ))
      | Call("ppmread", actuals) -> let a = (List.map expr actuals) in 
              SCall("ppmread", a,    (if (List.length actuals != 4) 
                                    then raise(Failure("ppmread only accepts 4 arguments"))
                                    else ( let a1 = List.hd actuals and a2 = expr (List.nth actuals 1) 
                                    and a3 = expr(List.nth actuals 2) and a4 = expr(List.nth actuals 3)
                                    in
                                    let t2 = Sast.get_expr_type_info a2 and t3 = Sast.get_expr_type_info a3
                                    and t4 = Sast.get_expr_type_info a4
                                    in
                                    let var2 = getString a2 and var3 = getString a3 and var4 = getString a4 in
                                        match (a1,t2,t3,t4) with
                                        StrLit(_),Mat(_,x2,y2),Mat(_,x3,y3),Mat(_,x4,y4) -> 
                                            if (x2!=x3)||(x3!=x4)||(y2!=y3)||(y3!=y4) then raise(Failure("All matrices must have the same dimensions")) else (); updateInitSyms var2; 
                                        updateInitSyms var3; updateInitSyms var4; Int
                                        | _ -> raise(Failure("ppmread takes string literal and 3 matrix types")) 
                                     ) ))
     | Call("pgmwrite", actuals) -> let a = (List.map expr actuals) in 
              SCall("pgmwrite", a,    (if (List.length actuals != 2) 
                                    then raise(Failure("pgmwrite only accepts 2 arguments"))
                                    else ( let a1 = List.hd actuals and a2 = expr (List.nth actuals 1) in
                                    let t2 = Sast.get_expr_type_info a2 in
                                    let var = getString a2 in
                                        match (a1,t2) with
                                        StrLit(_),Mat(_,_,_) -> updateInitSyms var; Void
                                        | _ -> raise(Failure("pgmwrite takes string literal and matrix type")) 
                                     ) ))
      | Call("ppmwrite", actuals) -> let a = (List.map expr actuals) in 
              SCall("ppmwrite", a,    (if (List.length actuals != 4) 
                                    then raise(Failure("ppmwrite only accepts 4 arguments"))
                                    else ( let a1 = List.hd actuals and a2 = expr (List.nth actuals 1) 
                                    and a3 = expr(List.nth actuals 2) and a4 = expr(List.nth actuals 3)
                                    in
                                    let t2 = Sast.get_expr_type_info a2 and t3 = Sast.get_expr_type_info a3
                                    and t4 = Sast.get_expr_type_info a4
                                    in
                                    let var2 = getString a2 and var3 = getString a3 and var4 = getString a4 in
                                        match (a1,t2,t3,t4) with
                                        StrLit(_),Mat(_,x2,y2),Mat(_,x3,y3),Mat(_,x4,y4) -> 
                                            if (x2!=x3)||(x3!=x4)||(y2!=y3)||(y3!=y4) then raise(Failure("All matrices must have the same dimensions")) else (); updateInitSyms var2; 
                                        updateInitSyms var3; updateInitSyms var4; Int
                                        | _ -> raise(Failure("ppmwrite takes string literal and 3 matrix types")) 
                                     ) ))
     | Call("print_board", actuals) -> let a = (List.map expr actuals) in 
              SCall("print_board", a,    (if (List.length actuals != 2) 
                                    then raise(Failure("Print_board only accepts 2 arguments"))
                                    else ( let a1 = expr(List.hd actuals) and a2 = expr (List.nth actuals 1) in
                                    let t1 = Sast.get_expr_type_info a1 in
                                    let t2 = Sast.get_expr_type_info a2 in
                                    let var = getString a1 in    
                                    match (t1, t2) with
                                        (Mat(_,_,_), Int) -> updateInitSyms var; Void
                                        | _ -> raise(Failure("Print_board takes string literal and matrix type")) 
                                     ) ))

      | Call(fname, actuals) as call -> let a = (List.map expr actuals) in
              SCall(fname, a, (let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et1 = expr e in
              let et = Sast.get_expr_type_info et1 in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ))
    in 
  
    let check_bool_expr e = if (Sast.get_expr_type_info (expr e)) != Bool
    then raise (Failure ("expected Boolean expression"))
     else (expr e) in
    
    (* Verify a statement or throw an exception *)
    let rec stmt = function
  Block sl ->(* SBlock(let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> (expr ())
        in check_block sl)*)
        SBlock(convertStmtToSStmt sl)
      | Expr e -> SExpr (expr e)
      | Return e -> SReturn(let t = expr e in 
                            let t1 = Sast.get_expr_type_info t in
                            if t1 = func.typ then expr e else
         raise (Failure ("return gives invalid type")))
           
      | If(p, b1, b2) -> SIf((check_bool_expr p),(stmt b1), (stmt b2))
      (*| For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st*)
      | While(p, s) -> SWhile((check_bool_expr p), stmt s)
    
  and convertStmtToSStmt sl = List.map stmt sl in
   let convertFdeclToSFdecl function_decls fdecl = 
    {
        sfname = fdecl.fname;
        styp   = fdecl.typ;
        sformals = fdecl.formals;
        slocals = fdecl.locals;
        sbody = (convertStmtToSStmt fdecl.body);
    }
   in
    let sast = convertFdeclToSFdecl function_decls func
    in

    (ignore(stmt (Block func.body)));sast

    in
   
    (List.map check_function functions)    
    (*
     List.map (fun x -> if ((List.length x.locals)==0) then raise(Failure("No Locals:"^x.fname)) 
     else check_function x) functions
    *)
