# aliases.ps1 - F# development aliases

function fslex {
    dotnet ..\..\fsLexer\FsLexYacc.11.3.0\build\fslex\net6.0\fslex.dll @args
}

function fsyacc {
    dotnet ..\..\fsLexer\FsLexYacc.11.3.0\build\fsyacc\net6.0\fsyacc.dll @args
}

function fsi {
    dotnet fsi -r ..\..\fsLexer/FsLexYacc.11.3.0/build/fsyacc/net6.0/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs @args
}

function gencomp {
    dotnet fsi -r ../../fsLexer/FsLexYacc.11.3.0/build/fsyacc/net6.0/FsLexYacc.Runtime.dll Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Interp.fs ParseAndRun.fs
}

function compuse {
dotnet fsi -r ../../fsLexer/FsLexYacc.11.3.0/build/fsyacc/net6.0/FsLexYacc.Runtime.dll Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs
}

function compuseback {
dotnet fsi -r ../../fsLexer/FsLexYacc.11.3.0/build/fsyacc/net6.0/FsLexYacc.Runtime.dll Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Contcomp.fs ParseAndContcomp.fs
}


Write-Host "F# aliases loaded: fslex, fsyacc, fsi, gencomp, compuse, compuseback" -ForegroundColor Green