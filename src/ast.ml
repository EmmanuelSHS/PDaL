(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not 

type expr =
    IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | DataframeLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type typ = Int | Float | Bool | String | None | Dataframe of typ * int 

type bind = typ * string

type var_decl = typ * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt * stmt
  | Elif of expr * stmt
  | Else of expr * stmt
  | For of expr * expr * stmt
  | While of expr * stmt 
  | In of expr * expr  
  | Declaration of var_decl
  | Break
  | Continue
  | Nostmt

type func_body = {
    f_vdecls: var_decl list;
    f_stmts: stmt list;
  }

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    fbody : func_body;
  }

type program = bind list * func_decl list

