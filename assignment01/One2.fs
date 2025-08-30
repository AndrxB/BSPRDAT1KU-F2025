(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)
module One2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _            -> failwith "unknown primitive";;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

(* 
  (i)
  Declare an alternative datatype aexpr for a representation of arithmetic ex-
  pressions without let-bindings. The datatype should have constructors 
  CstI, Var, Add, Mul, Sub, for constants, variables, addition, multiplication, 
  and subtraction. Then x ∗ (y + 3) is represented as 
  Mul(Var "x", Add(Var "y", CstI 3)), 
  not as Prim("*", Var "x", Prim("+", Var "y", CstI 3))
*)

type aexpr1 =
  | CstI of int
  | Var of string
  | Add of aexpr1 * aexpr1
  | Mul of aexpr1 * aexpr1
  | Sub of aexpr1 * aexpr1;;

(*
  (ii)
  Write some example expressions in this extended expression language, 
  using abstract syntax, and evaluate them using your new eval function.

  v-(w+z) = Sub(Var "v", Add(Var "w", Var "z"))
  2*(v-(w+z)) = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))
  x + y + z + v = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))
*)

let ae1 = Sub(Var "v", Add(Var "w", Var "z"));;
let ae2 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")));;
let ae3 = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")));;

(*
  (iii)
  Write an F# function fmt : aexpr -> string to format expressions as strings. 
  For instance, it may format Sub(Var "x", CstI 34) as the string "(x - 34)". 
  It has very much the same structure as an eval func- tion, but takes no 
  environment argument (because the name of a variable is independent of its value).
*)
let rec fmt (e : aexpr1) : string =
  match e with
    | CstI i -> i.ToString()
    | Var x -> x
    | Add(e1, e2) -> sprintf "(%s + %s)" (fmt e1) (fmt e2)
    | Mul(e1, e2) -> sprintf "(%s * %s)" (fmt e1) (fmt e2)
    | Sub(e1, e2) -> sprintf "(%s - %s)" (fmt e1) (fmt e2)

(*
  (iv)
  Write an F# function simplify : aexpr -> aexpr to perform expres- sion simplification. 
  For instance, it should simplify (x + 0) to x, and simplify (1 + 0) to 1. 
  The more ambitious student may want to simplify (1 + 0) ∗ (x + 0) to x. 
  Hint: Pattern matching is your friend. Hint: Don’t forget the case where you cannot 
  simplify anything.
*)
let rec simplify (e : aexpr1) : aexpr1 =
  match e with
    | Add(CstI 0, e) -> e
    | Add(e, CstI 0) -> e
    | Sub(e, CstI 0) -> e
    | Mul(CstI 1, e) -> e
    | Mul(e, CstI 1) -> e
    | Mul(CstI 0, e) -> CstI 0
    | Mul(e, CstI 0) -> CstI 0
    | Sub(e1, e2) when e1 = e2 -> CstI 0
    | _ -> e

(*
  (v)
  Write an F# function to perform symbolic differentiation of simple 
  arithmetic expressions (such as aexpr) with respect to a single variable.
*)
type dexpr =
  | CstI of int
  | Var of string
  | Add of dexpr * dexpr
  | Sub of dexpr * dexpr
  | Mul of dexpr * dexpr;;

let rec diff e : dexpr =
  match e with
    | CstI i -> CstI i
    | Var x -> Var x
    | Add(e1, e2) -> Add(diff e1, diff e2)
    | Sub(e1, e2) -> Sub(diff e1, diff e2)
    | Mul(e1, e2) -> Add(Mul(diff e1, e2), Mul(e1, diff e2));;