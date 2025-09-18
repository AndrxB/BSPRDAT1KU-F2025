# aliases.ps1 - F# development aliases

function fslex {
    dotnet ..\fsLexer\FsLexYacc.11.3.0\build\fslex\net6.0\fslex.dll @args
}

function fsyacc {
    dotnet ..\fsLexer\FsLexYacc.11.3.0\build\fsyacc\net6.0\fsyacc.dll @args
}

function fsi {
    dotnet fsi -r ..\fsLexer/FsLexYacc.11.3.0/build/fsyacc/net6.0/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs @args
}

Write-Host "F# aliases loaded: fslex, fsyacc, fsi" -ForegroundColor Green