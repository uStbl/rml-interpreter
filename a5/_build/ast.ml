(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, handle, uop, bop) are used by the parser and type-checker.
   You do not want to change them.
 ******************************************************************************)

type id = string

type handle = int

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Cat
  | Pipe
  | Cons
  | Assign
  | Bind

and uop =
  | Neg
  | Not
  | Ref
  | Deref


(******************************************************************************
   [pat] is the type of the AST for patterns. You may implement
   this type however you wish. Look at the formal semantics and think about other
   AST types we have worked with for inspiration.
 ******************************************************************************)

type pat =
  | Var of string
  (* | WildP
     | ConstantP
     | VarP
     | PairP *)

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr =
  | Unit of unit
  | Int of int
  | String of string
  | Bool of bool
  | VarE of string
  | Bop of bop * expr * expr
  | Let of pat * expr * expr
  | Fun of pat * expr
  | App of expr * expr
  | Print of expr
  | Println of expr
  | If of expr * expr * expr
  (* | Var of string
     | Pair of expr * expr
     | Empty of expr list
     | List of expr list *)


(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement
   this type however you wish.  There is only one kind of
   definition---the let [rec] definition---so this type can be quite
   simple.
 ******************************************************************************)

and defn = pat * expr

(******************************************************************************
   [prog] is the type of the AST for an RML program. You should 
   not need to change it.
 ******************************************************************************)

type prog = defn list
