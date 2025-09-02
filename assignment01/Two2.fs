(* Programming language concepts for software developers, 2012-02-17 *)

(* Evaluation, checking, and compilation of object language expressions *)
(* Stack machines for expression evaluation                             *) 

(* Object language expressions with variable bindings and nested scope *)

module Two2


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
let e12 = Let1([("x1", CstI1 2); ("x2", CstI1 0)], Prim1("-", Var1 "x1", Var1 "x2"))
let e13 = Let1(["x1", Prim1("+", Var1 "x1", CstI1 7)], Prim1("+", Var1 "x1", CstI1 8))

let e14 = Let1(["x1", Prim1("+", CstI1 5, CstI1 7); "x2", Prim1("*", Var1 "x1", CstI1 2)], Prim1("+", Var1 "x1", Var1 "x2"))


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
    | Var1 x             ->
        lookup env x 
    | Let1(xlist, ebody) -> 
        let x, erhs = xlist.Head
        let xval = eval1 erhs env
        let env1 = (x, xval) :: env
        match xlist with
        | [_, _] ->
            eval1 ebody env1
        | _::_ ->
            eval1 (Let1(xlist.Tail, ebody)) env1
        | _ -> failwith "No let-bindings defined"
    | Prim1("+", e1, e2) -> eval1 e1 env + eval1 e2 env
    | Prim1("*", e1, e2) -> eval1 e1 env * eval1 e2 env
    | Prim1("-", e1, e2) -> eval1 e1 env - eval1 e2 env
    | Prim1 _            -> failwith "unknown primitive";;

let run e = eval e [];;
let res = List.map run [e1;e2;e3;e4;e5;e7]  (* e6 has free variables *)

let run1 e = eval1 e [];;

let res1 = List.map run1 [e11;e12]

(* ---------------------------------------------------------------------- *)

(* Closedness *)

// let mem x vs = List.exists (fun y -> x=y) vs;;

let rec mem x vs = 
    match vs with
    | []      -> false
    | v :: vr -> x=v || mem x vr;;

(* Free variables *)

(* Operations on sets, represented as lists.  Simple but inefficient;
   one could use binary trees, hashtables or splaytrees for
   efficiency.  *)

(* union(xs, ys) is the set of all elements in xs or ys, without duplicates *)

let rec union (xs, ys) = 
    match xs with 
    | []    -> ys
    | x::xr -> if mem x ys then union(xr, ys)
               else x :: union(xr, ys);;

(* minus xs ys  is the set of all elements in xs but not in ys *)

let rec minus (xs, ys) = 
    match xs with 
    | []    -> []
    | x::xr -> if mem x ys then minus(xr, ys)
               else x :: minus (xr, ys);;

(* Find all variables that occur free in expression e *)

let rec freevars e : string list =
    match e with
    | CstI _ -> []
    | Var x  -> [x]
    | Let(x, erhs, ebody) -> 
          union (freevars erhs, minus (freevars ebody, [x]))
    | Prim(_, e1, e2) -> union (freevars e1, freevars e2);;



(*Revise the function freevars : expr -> string list to
work for the language as extended in Exercise 2.1. 
Note that the example expression
in the beginning of Exercise 2.1 has no free variables, but let x1 = x1+7 in
x1+8 end has the free variable x1, 
because the variable x1 is bound only in the
body (x1+8), not in the right-hand side (x1+7), of its own binding. 
There are programming languages where a variable can be used in the right-hand side of its
own binding, but ours is not such a language.*)

let rec freevars1 (e: expr1) : string list =
    match e with
    | CstI1 _ -> []
    | Var1 x  -> [x]
    | Let1(xlist, ebody) ->
        let x, erhs = xlist.Head
        match xlist with
        | [_, _] ->
            union (freevars1 erhs, minus (freevars1 ebody, [x]))
        | _::_ ->
            union (freevars1 erhs, minus (freevars1(Let1(xlist.Tail, ebody)), [x]))
        | _ -> failwith "No let-bindings defined"
    | Prim1(_, e1, e2) -> union (freevars1 e1, freevars1 e2);;


(* Alternative definition of closed *)

let closed e = (freevars e = []);;
let hej1 = List.map closed [e1;e2;e3;e4;e5;e6;e7;e8;e9;e10]


let closed1 e = (freevars1 e = []);; 
let hej = List.map closed1 [e11;e12;e13;e14] // False if there are free variables, true otherwise

(* ---------------------------------------------------------------------- *)