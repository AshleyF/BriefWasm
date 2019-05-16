printfn "Hello WASM World!"

open Utility
open Structure
open Encoding
open Brief
open Tests

testAll ()

printfn "Building module..."

let funcType, funcBody, globals = word Map.empty "glob@ 0 drop arg@ 0 123 + loc! 1 3 4 + 5 * loc@ 1 +" 

let bytes =
    wasm [
        Type [funcType]
        Function [0]
        Global [{ Value = Value.I32; Mutable = false; Init = [ConstI32 123; End]}]
        Export [{ Field = "main"; Kind = ExternalKind.Function; Index = 0 }]
        Code [funcBody]
    ]

save bytes