(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)
module One1

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
  File Intro2.fs contains a definition of the expr expression language and 
  an evaluation function eval. Extend the eval function to handle three addi-
  tional operators: "max", "min", and "==". Like the existing operators, they 
  take two argument expressions. The equals operator should return 1 when 
  true and 0 when false.
*)
let rec eval1 e (env : (string * int) list) : int =
    match e with
    | CstI i              -> i
    | Var x               -> lookup env x
    | Prim("+", e1, e2)   -> eval1 e1 env + eval1 e2 env
    | Prim("*", e1, e2)   -> eval1 e1 env * eval1 e2 env
    | Prim("-", e1, e2)   -> eval1 e1 env - eval1 e2 env
    | Prim("min", e1, e2) ->
      let e1', e2' = eval1 e1 env, eval1 e2 env
      if e1' >= e2' then e2' else e1'
    | Prim("max", e1, e2) ->
      let e1', e2' = eval1 e1 env, eval1 e2 env
      if e1' >= e2' then e1' else e2'
    | Prim("==", e1, e2)  -> if eval1 e1 env = eval1 e2 env then 1 else 0
    | Prim _              -> failwith "unknown primitive";;

(*
  (ii)
  Write some example expressions in this extended expression language, 
  using abstract syntax, and evaluate them using your new eval function.
*)
let e11 = CstI 1000000
let e12 = CstI 20
let e13 = Prim("==", e1, e2) // e1 is 14, e2 is 'a' + 3
let e14 = Prim("-", e2, e13)

let evale11 = eval1 e11 env;;

let evale13 = eval1 e13 [("a", 14)];;

let evale14 = eval1 e14 [("a", 14)];;


(*
  (iii)
  Rewrite one of the eval functions to evaluate the arguments of a 
  primitive before branching out on the operator, in this style:
    let rec eval e (env : (string * int) list) : int = 
      match e with 
      | ... 
      | Prim(ope, e1, e2) -> 
        let i1 = ... 
        let i2 = ... 
        match ope with 
        | "+" -> i1 + i2 
        | ...
*)
let rec eval2 e (env : (string * int) list) : int =
  match e with
    | CstI i    -> i
    | Var x  -> lookup env x
    | Prim(ope, e1, e2) ->
      let i1, i2 = eval2 e1 env, eval2 e2 env
      match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "min" -> if i1 >= i2 then i2 else i1
        | "max" -> if i1 >= i2 then i1 else i2
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith "unknown operator";;

let eval2e14 = eval2 e14 [("a", 14)];;


(*
  (iv)
  Extend the expression language with conditional expressions If(e1, e2, e3) 
  corresponding to Java’s expression e1 ? e2 : e3 or F#’s conditional expression 
  if e1 then e2 else e3. You need to extend the expr datatype with a new constructor 
  If that takes three expr arguments.
*)
type expr1 =
  | CstI of int
  | Var of string
  | Prim of string * expr1 * expr1
  | If of expr1 * expr1 * expr1;;

(*
  (v)
  Extend the interpreter function eval correspondingly. It should evaluate e1, 
  and if e1 is non-zero, then evaluate e2, else evaluate e3. You should be able 
  to evaluate the expression If(Var "a", CstI 11, CstI 22) in an environment 
  that binds variable a.
*)
let rec eval3 (e: expr1) (env : (string * int) list) : int =
  match e with
    | CstI i                            -> i
    | Var x                          -> lookup env x
    | If(e1, e2, e3)     -> if eval3 e1 env <> 0 then eval3 e2 env else eval3 e3 env
    | Prim(ope, e1, e2) ->
      let i1, i2 = eval3 e1 env, eval3 e2 env
      match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "min" -> if i1 >= i2 then i2 else i1
        | "max" -> if i1 >= i2 then i1 else i2
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith "unknown operator";;
