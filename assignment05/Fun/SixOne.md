# program 3
When parsing and running the third program through the evaluator. The evaluator will output the following:
```fs
val it: expr =
  Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Let
       ("addtwo", Call (Var "add", CstI 2),
        Let ("x", CstI 77, Call (Var "addtwo", CstI 5))))
```
Which is the expected abstract syntax for the third program, but if it executed then it will, then it will give a an exception. Since `add` only has 1 parameter meaning that the second param is defined, when running it.
# program 4
The result from the fourth programs is this:
```fs
val it: expr =
  Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Call (Var "add", CstI 2))
```
Here is a function called add defined with a parameter with called x. The body of this function is inside of LetFun, which defined as function called f that if called adds x and y together. And lastly after defining the add function, it gets called with a 2 as the parameter.