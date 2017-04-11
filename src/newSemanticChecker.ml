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
	|Float -> requireFloats tlist "All of tlist must be Floats"
	|_ -> raise(Failure ("Invalid first type in checkNums")

let checkArithBinop t1 op t2 = match op with
	Add|Sub|Mult|Div -> (let _ = checkNums[t1;t2] in t1)
	|_ -> raise (Failure ("Invalid operand in checkArithBinop")




(*This returns the type of e and raises a flag if inconsistent or invalid syntax*)
 let checkType e = function
	 IntLit _ -> Int
	|FloatLit _ -> Float
	|BoolLit _ -> Bool
	|MatrixLit m -> checkMatrixLiterals m 	(*To be done later*)
	|Id s -> getIDType s
