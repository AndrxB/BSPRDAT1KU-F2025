(* Programming language concepts for software developers, 2012-02-17 *)

(* Evaluation, checking, and compilation of object language expressions *)
(* Stack machines for expression evaluation                             *) 

(* Object language expressions with variable bindings and nested scope *)

module Two2

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
let e12 = Let1([("x1", CstI1 2); ("x2", CstI1 0)], Prim1("-", Var1 "x1", Var1 "x2"))



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
        let (x, erhs) = xlist.Head
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

(* Checking whether an expression is closed.  The vs is 
   a list of the bound variables.  *)

let rec closedin (e : expr) (vs : string list) : bool =
    match e with
    | CstI i -> true
    | Var x  -> List.exists (fun y -> x=y) vs
    | Let(x, erhs, ebody) -> 
      let vs1 = x :: vs 
      closedin erhs vs && closedin ebody vs1
    | Prim(ope, e1, e2) -> closedin e1 vs && closedin e2 vs;;

(* An expression is closed if it is closed in the empty environment *)

let closed1 e = closedin e [];;
let _ = List.map closed1 [e1;e2;e3;e4;e5;e6;e7;e8;e9;e10]

(* ---------------------------------------------------------------------- *)

(* Substitution of expressions for variables *)

(* This version of lookup returns a Var(x) expression if there is no
   pair (x,e) in the list env --- instead of failing with exception: *)

let rec lookOrSelf env x =
    match env with 
    | []        -> Var x
    | (y, e)::r -> if x=y then e else lookOrSelf r x;;

(* Remove (x, _) from env: *)

let rec remove env x =
    match env with 
    | []        -> []
    | (y, e)::r -> if x=y then r else (y, e) :: remove r x;;

(* Naive substitution, may capture free variables: *)

let rec nsubst (e : expr) (env : (string * expr) list) : expr =
    match e with
    | CstI i -> e
    | Var x  -> lookOrSelf env x
    | Let(x, erhs, ebody) ->
      let newenv = remove env x
      Let(x, nsubst erhs env, nsubst ebody newenv)
    | Prim(ope, e1, e2) -> Prim(ope, nsubst e1 env, nsubst e2 env)

(* Some expressions with free variables: *)

let e6s0 = Prim("+", Var "y", Var "z");;

let e6s1 = nsubst e6s0 [("z", CstI 17)];;

let e6s2 = nsubst e6s0 [("z", Prim("-", CstI 5, CstI 4))];;

let e6s3 = nsubst e6s0 [("z", Prim("+", Var "z", Var "z"))];;

// Shows that only z outside the Let gets substituted:
let e7s0 = Prim("+", Let("z", CstI 22, Prim("*", CstI 5, Var "z")),
                   Var "z");;

let e7s1 = nsubst e7s0 [("z", CstI 100)];;

// Shows that only the z in the Let rhs gets substituted
let e8s0 = Let("z", Prim("*", CstI 22, Var "z"), Prim("*", CstI 5, Var "z"));;

let e8s1 = nsubst e8s0 [("z", CstI 100)];;

// Shows (wrong) capture of free variable z under the let:
let e9s0 = Let("z", CstI 22, Prim("*", Var "y", Var "z"));;

let e9s1 = nsubst e9s0 [("y", Var "z")];;

// 
let e9s2 = nsubst e9s0 [("z", Prim("-", CstI 5, CstI 4))];;

let newVar : string -> string = 
    let n = ref 0
    let varMaker x = (n := 1 + !n; x + string (!n))
    varMaker

(* Correct, capture-avoiding substitution *)

let rec subst (e : expr) (env : (string * expr) list) : expr =
    match e with
    | CstI i -> e
    | Var x  -> lookOrSelf env x
    | Let(x, erhs, ebody) ->
      let newx = newVar x
      let newenv = (x, Var newx) :: remove env x
      Let(newx, subst erhs env, subst ebody newenv)
    | Prim(ope, e1, e2) -> Prim(ope, subst e1 env, subst e2 env)

let e6s1a = subst e6 [("z", CstI 17)];;

let e6s2a = subst e6 [("z", Prim("-", CstI 5, CstI 4))];;

let e6s3a = subst e6 [("z", Prim("+", Var "z", Var "z"))];;


// Shows renaming of bound variable z (to z1)
let e7s1a = subst e7s0 [("z", CstI 100)];;

// Shows renaming of bound variable z (to z2)
let e8s1a = subst e8s0 [("z", CstI 100)];;

// Shows renaming of bound variable z (to z3), avoiding capture of free z
let e9s1a = subst e9s0 [("y", Var "z")];;

(* ---------------------------------------------------------------------- *)

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
    | CstI i -> []
    | Var x  -> [x]
    | Let(x, erhs, ebody) -> 
          union (freevars erhs, minus (freevars ebody, [x]))
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2);;

(* Alternative definition of closed *)

let closed2 e = (freevars e = []);;
let _ = List.map closed2 [e1;e2;e3;e4;e5;e6;e7;e8;e9;e10]

(* ---------------------------------------------------------------------- *)