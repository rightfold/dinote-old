$ErrorActionPreference = "Stop"

Get-ChildItem src -Recurse -Filter *.purspg `
| ForEach-Object {
    perl6 `
        bower_components/purescript-postgresql-client/purspgpp `
        "user=postgres password=lol123 dbname=nn" `
        "$($_.FullName)" `
        "$([IO.Path]::ChangeExtension($_.FullName, "purs"))"
    if (!$?) {
        Write-Error "Unable to preprocess $_"
    }
}

pulp browserify --main Main.Client > output/nn.js
if (!$?) {
    Write-Error "Unable to compile"
}
