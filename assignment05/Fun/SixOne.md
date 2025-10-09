# Exercise 6.1
Download and unpack fun1.zip and fun2.zip and build the
micro-ML higher-order evaluator as described in file README.TXT point E.
Then run the evaluator on the following four programs. Is the result of the third
one as expected? Explain the result of the last one:

## program 3
```fs
let add x = let f y = x+y in f end
in let addtwo = add 2
in addtwo 5 end
end
```

When parsing and running the third program through the evaluator. The evaluator will output the following:
```fs
val it: expr =
  Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Let
       ("addtwo", Call (Var "add", CstI 2),
        Let ("x", CstI 77, Call (Var "addtwo", CstI 5))))
```
Which is the expected abstract syntax for the third program, but if it executed then it will, then it will give a an exception. Since `add` only has 1 parameter meaning that the second param is defined, in runtime.

## program 4
```fs
let add x = let f y = x+y in f end
in let addtwo = add 2
in addtwo 5 end
end
```

The result from the fourth programs is this:
```fs
val it: expr =
  Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Call (Var "add", CstI 2))
```
Here is a function called add defined with a parameter with called x. The body of this function is inside of LetFun, which defined as function called f that if called adds x and y together. And lastly after defining the add function, it gets called with a 2 as the parameter.