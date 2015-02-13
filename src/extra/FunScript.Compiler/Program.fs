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
let projectPath = ref ""
let outPath = ref ""
let stdOut = ref false

let args =
    [ ArgInfo("--assembly-path", ArgType.String(fun x -> assemPath := x), 
              "Path to the assembly you want fun script to compile")
      ArgInfo("--project-path", ArgType.String(fun x -> projectPath := x), 
              "Path to the project you want fun script to compile")
      ArgInfo("--out-path", ArgType.String(fun x -> outPath := x), 
              "Path of the resulting javascript file")
      ArgInfo("--std-out", ArgType.SetArg(stdOut), 
              "Write the results to standard out instead") ]

let usageText = "FunScript Compiler - usage: funsc <args>"

let emptyStr = String.IsNullOrWhiteSpace

let (|EmptyString|NonEmptyString|) 
    input = if System.String.IsNullOrWhiteSpace input then EmptyString else NonEmptyString

type ParseResult = {Success : bool; Message : string}

[<EntryPoint>]
let main argv = 
    ArgParser.Parse(args, usageText = usageText)
    
    let sourceParseResult : ParseResult = 
        match !assemPath, !projectPath with
        | (EmptyString, EmptyString) -> 
            {Success=false; Message="error: either --assembly-path or --project-path is required"}
        | (NonEmptyString, NonEmptyString) -> 
            {Success=false; Message="error: both --assembly-path and --project-path specified. one or the other required"}
        | (_, _) -> 
            {Success=true; Message=""}

    let outParseResult : ParseResult = 
        match !outPath, !stdOut with
        | (EmptyString, false) -> 
            {Success=false; Message="error: either --out-path or --std-out is required"}
        | (NonEmptyString, true) -> 
            {Success=false; Message="error: both --out-path and --std-out specified. one or the other is required"}
        | (_, _) -> 
            {Success=true; Message=""}

    let parseResults = [sourceParseResult; outParseResult]
    let parseFailMessages = seq { for r in parseResults do if not(r.Success) then yield r.Message}

    let mutable exitCode = 0

    if not(Seq.isEmpty(parseFailMessages)) then
        for m in parseFailMessages do   
            printf "%s\n" m
        ArgParser.Usage(args, usage = usageText)
        exitCode <- 1
    else 
        compile !assemPath !outPath []//Interop.Components.all
        
    exitCode