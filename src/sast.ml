(*
 * File: parser.mly
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
  | SMatrixLit of expr list list * typ
  | SId of string * typ
  | SStrLit of string
  | SBinop of expr * op * expr * typ
  | SUnop of uop * expr * typ
  | SAssign of string * expr
  | SCall of string * expr list
  | SNoexpr
  | SNull
  | SMatrixAccess of string * int * int * typ

type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	(*| SFor of sexpr * sexpr * sexpr * sstmt*)
	| SWhile of sexpr * sstmt
	| SReturn of sexpr

type sfunc_decl = {
	sreturn_type 	: datatype;
	sfname 			: string;
	sformals 		: formal list;
	slocals  		: local list;
	sbody 			: sstmt list;
}

type sprogram = func_decl list