(* Programming language concepts for software developers, 2012-02-17 *)

(* Evaluation, checking, and compilation of object language expressions *)
(* Stack machines for expression evaluation                             *) 

(* Object language expressions with variable bindings and nested scope *)

module Two1

(* (i) Extend the expression language expr from Intcomp1.fs with
multiple sequential let-bindings*)

type expr1 =
    | CstI1 of int
    | Var1 of string
    | Let1 of (string * expr1) list * expr1
    | Prim1 of string * expr1 * expr1;;

type expr = 
  | CstI of int
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr;;




(* Some closed expressions: *)

let e1 = Let("z", CstI 17, Prim("+", Var "z", Var "z"));;

let e2 = Let("z", CstI 17, 
             Prim("+", Let("z", CstI 22, Prim("*", CstI 100, Var "z")),
                       Var "z"));;

let e3 = Let("z", Prim("-", CstI 5, CstI 4), 
             Prim("*", CstI 100, Var "z"));;

let e4 = Prim("+", Prim("+", CstI 20, Let("z", CstI 17, 
                                          Prim("+", Var "z", CstI 2))),
                   CstI 30);;

let e5 = Prim("*", CstI 2, Let("x", CstI 3, Prim("+", Var "x", CstI 4)));;

let e6 = Let("z", Var "x", Prim("+", Var "z", Var "x"))
let e7 = Let("z", CstI 3, Let("y", Prim("+", Var "z", CstI 1), Prim("+", Var "z", Var "y")))
let e8 = Let("z", Let("x", CstI 4, Prim("+", Var "x", CstI 5)), Prim("*", Var "z", CstI 2))
let e9 = Let("z", CstI 3, Let("y", Prim("+", Var "z", CstI 1), Prim("+", Var "x", Var "y")))
let e10 = Let("z", Prim("+", Let("x", CstI 4, Prim("+", Var "x", CstI 5)), Var "x"), Prim("*", Var "z", CstI 2))


let e11 = Let1 ([("x1", CstI1 2); ("x2", CstI1 3)], Prim1("+", Var1 "x1", Var1 "x2"))

(* ---------------------------------------------------------------------- *)




(* Evaluation of expressions with variables and bindings *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let rec eval (e: expr) (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Let(x, erhs, ebody) -> 
      let xval = eval erhs env
      let env1 = (x, xval) :: env 
      eval ebody env1
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _            -> failwith "unknown primitive";;

let rec eval1 (e: expr1) (env : (string * int) list) : int =
    match e with
    | CstI1 (i: int)            -> i
    | Var1 x             -> lookup env x 
    | Let1(xlist, ebody) -> 
        let (x, erhs) = xlist.Head
        let xval = eval1 erhs env
        let env1 = (x, xval) :: env
        match xlist with
        | [_, _] -> eval1 ebody env1
        | _::_ -> eval1 (Let1(xlist.Tail, ebody)) env1
        | _ -> failwith "No let-bindings defined"
    | Prim1("+", e1, e2) -> eval1 e1 env + eval1 e2 env
    | Prim1("*", e1, e2) -> eval1 e1 env * eval1 e2 env
    | Prim1("-", e1, e2) -> eval1 e1 env - eval1 e2 env
    | Prim1 _            -> failwith "unknown primitive";;

let run e = eval e [];;
let res = List.map run [e1;e2;e3;e4;e5;e7]  (* e6 has free variables *)

let run1 e = eval1 e [];;

let res1 = List.map run1 [e11;]

(* ---------------------------------------------------------------------- *)
