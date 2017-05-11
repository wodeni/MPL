(*
 * File: sast.mly
 * Date: 2017-04-11
 *
 * PLT Spring 2017
 * MPL Project
 * Wode "Nimo" Ni    <wn2155@columbia.edu>
 * David Rincon-Cruz <dr2884@columbia.edu>
 * Chi Zhang         <cz2440@columbia.edu>
 * Jiangfeng Wang    <jw3107@columbia.edu>
 *)

open Ast

type sexpr =
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SStrLit of string
  | SMatrixLit of sexpr list list * typ
  | SFMatrixLit of string list list * typ
  | SId of string * typ
  | SBinop of sexpr * op * sexpr * typ
  | SUnop of uop * sexpr * typ
  | SAssign of string * sexpr * typ
  | SCall of string * sexpr list * typ
  | SNull of typ
  | SMatrixAccess of string * sexpr * sexpr * typ

let get_expr_type_info sexpr = match sexpr with
   |SIntLit _ ->                Int
   |SFloatLit _ ->              Float
   |SBoolLit _ ->               Bool
   |SMatrixLit (_,x) ->         x
   |SFMatrixLit(_,x) ->         x
   |SId (_,x) ->                x
   |SStrLit _ ->                String
   |SBinop (_,_,_,x) ->         x
   |SUnop (_,_,x) ->            x
   |SAssign (_,_,x) ->          x
   |SCall (_,_,x) ->            x
   |SNull x ->                  x
   |SMatrixAccess (_,_,_,x) ->  x


type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	(*| SFor of sexpr * sexpr * sexpr * sstmt*)
	| SWhile of sexpr * sstmt
	| SReturn of sexpr

type sfunc_decl = {
	styp         	: typ;
	sfname 			: string;
	sformals 		: bind list;
	slocals  		: bind list;
	sbody 			: sstmt list;
}

type sprogram = sfunc_decl list
