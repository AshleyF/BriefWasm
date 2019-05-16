module Utility

open System
open System.IO

let save (wasm: byte seq) =
    let bytes = Array.ofSeq wasm
    use binWrite = File.Create("./main.wasm")
    binWrite.Write(bytes, 0, bytes.Length)
    let base64 = Convert.ToBase64String bytes
    use jsWriter = File.CreateText("./main.js")
    jsWriter.WriteLine(sprintf "run('%s');" base64)
