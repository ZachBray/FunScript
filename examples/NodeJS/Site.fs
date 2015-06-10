[<ReflectedDefinition>]
module Program

open System.IO
open FunScript
open FunScript.TypeScript
open FunScript.Compiler

// Imports
// Note: this path is relative to the directory you launched mono / monodevelop.
type ts = Api< @"Examples/Typings/node-0.8.d.ts" >

// Server script
let server() =
   // Note: magic variable name! The typings file isn't quite correct.
   // It defines http as a static object rather than a type.
   let http = unbox<ts.http>(ts.require.Invoke "http")
   let port = 8124.
   ts.http
      .createServer(fun req res ->
         let stream = res.AsWritableStream()
         res.writeHead 200.
         res.``end`` "hello world!")
      .listen(8124.)
   ts.console.info [| "listening on port: " + port.ToString() |]


// Compile to JS:
let filePath = "server.js"
let serverScript = Compiler.Compile(<@ server() @>, components=Interop.Components.all, noReturn=true)
File.Delete(filePath)
File.WriteAllText(filePath, serverScript)
printfn "%s updated!" filePath