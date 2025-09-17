(* File Expr/Parse.fs *)
(* Lexing and parsing of simple expressions using fslex and fsyacc *)
module Parse

open System
open System.IO
open System.Text
open FSharp.Text.Lexing
open Absyn
open Expr

(* Plain parsing from a string, with poor error reporting *)

let fromString (str : string) : expr =
    let lexbuf = LexBuffer<char>.FromString(str)
    try 
      ExprPar.Main ExprLex.Token lexbuf
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s near line %d, column %d\n" 
                  (exn.Message) (pos.Line+1) pos.Column
             
(* Parsing from a text file *)

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader reader
    try 
      ExprPar.Main ExprLex.Token lexbuf
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s in file %s near line %d, column %d\n" 
                  (exn.Message) filename (pos.Line+1) pos.Column

(* 
Exercise 3.6 Use the expression parser from Parse.fs and the compiler scomp
(from expressions to stack machine instructions) and the associated datatypes from
Expr.fs, to define a function compString : string -> sinstr list
that parses a string as an expression and compiles it to stack machine code.
 *)
let compString (s: string): sinstr list = 
  scomp (fromString s) [];;