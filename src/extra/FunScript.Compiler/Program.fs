open System
open System.IO
open System.Reflection
open FunScript
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core;
open Microsoft.FSharp.Text;

let compile assemblyPath filename components =

    // loading the assembly from a file maybe problematic if 
    // the assembly as dependcies on non-BCL stuff, consider
    // otherways of doing this
    let asm = Assembly.LoadFile(assemblyPath)

    // Find the main method in this assembly
    // Could offer lot more flexiblity here ...
    let mainCompileExpr =
        printfn "Searching for main function..."
        let types = asm.GetTypes()
        let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
        let mains = 
            [ for typ in types do
                for mi in typ.GetMethods(flags) do
                    if mi.Name = "main" then yield mi ]
        let main = 
            match mains with
            | [it] -> it
            | _ -> failwith "Main function not found!"
        printfn "Found entry point..."
        Expr.Call(main, [])


    // Compile the main function into a script
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let source = FunScript.Compiler.Compiler.Compile(mainCompileExpr, components=components)
    let sourceWrapped = sprintf "$(document).ready(function () {\n%s\n});" source
    printfn "Generated JavaScript in %f sec..." (float sw.ElapsedMilliseconds / 1000.0) 
    File.Delete filename
    File.WriteAllText(filename, sourceWrapped)


let assemPath = ref ""
let outPath = ref ""
let args =
    [ ArgInfo("--assembly-path", ArgType.String(fun x -> assemPath := x), 
              "Path to the assembly you want fun script to compile")
      ArgInfo("--out-path", ArgType.String(fun x -> outPath := x), 
              "Path of the resulting javascript file") ]

let usageText = "FunScript Compiler - usage: funsc <args>"

[<EntryPoint>]
let main argv = 
    ArgParser.Parse(args, usageText = usageText)
    if String.IsNullOrWhiteSpace(!assemPath) || String.IsNullOrWhiteSpace(!outPath) then
        printfn "error: both --assembly-path and --out-path are required"
        ArgParser.Usage(args, usage = usageText)
    else
        // need to give more control of the components?
        compile !assemPath !outPath []//Interop.Components.all
    0
