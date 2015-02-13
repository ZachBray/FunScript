open System
open System.IO
open System.Reflection
open FunScript
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core;
open Microsoft.FSharp.Text;
//
//let compile assemblyPath filename components =
//
//    // loading the assembly from a file maybe problematic if 
//    // the assembly as dependcies on non-BCL stuff, consider
//    // otherways of doing this
//    let asm = Assembly.LoadFile(assemblyPath)
//
//    // Find the main method in this assembly
//    // Could offer lot more flexiblity here ...
//    let mainCompileExpr =
//        printfn "Searching for main function..."
//        let types = asm.GetTypes()
//        let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
//        let mains = 
//            [ for typ in types do
//                for mi in typ.GetMethods(flags) do
//                    if mi.Name = "main" then yield mi ]
//        let main = 
//            match mains with
//            | [it] -> it
//            | _ -> failwith "Main function not found!"
//        printfn "Found entry point..."
//        Expr.Call(main, [])
//
//    // Compile the main function into a script
//    let sw = System.Diagnostics.Stopwatch.StartNew()
//    let source = FunScript.Compiler.Compiler.Compile(mainCompileExpr, components=components)
//    let sourceWrapped = sprintf "$(document).ready(function () {\n%s\n});" source
//    printfn "Generated JavaScript in %f sec..." (float sw.ElapsedMilliseconds / 1000.0) 
//    File.Delete filename
//    File.WriteAllText(filename, sourceWrapped)


let assemPathRef = ref ""
let projectPathRef = ref ""
let outPathRef = ref ""
let stdOutRef = ref false

let args =
    [ ArgInfo("--assembly-path", ArgType.String(fun x -> assemPathRef := x), 
              "Path to the assembly you want fun script to compile")
      ArgInfo("--project-path", ArgType.String(fun x -> projectPathRef := x), 
              "Path to the project you want fun script to compile")
      ArgInfo("--out-path", ArgType.String(fun x -> outPathRef := x), 
              "Path of the resulting javascript file")
      ArgInfo("--std-out", ArgType.SetArg(stdOutRef), 
              "Write the results to standard out instead") ]

let usageText = "FunScript Compiler - usage: funsc <args>"

let emptyStr = String.IsNullOrWhiteSpace

let (|EmptyString|NonEmptyString|) 
    input = if System.String.IsNullOrWhiteSpace input then EmptyString else NonEmptyString

[<EntryPoint>]
let main argv = 
    ArgParser.Parse(args, usageText = usageText)

    let assemPath = !assemPathRef
    let projectPath = !projectPathRef
    let outPath = !outPathRef
    let stdOut = !stdOutRef
    
    let sourceParseResult = 
        match assemPath, projectPath with
        | (EmptyString, EmptyString) -> 
            Some "error: either --assembly-path or --project-path is required"
        | (NonEmptyString, NonEmptyString) -> 
            Some "error: both --assembly-path and --project-path specified. one or the other required"
        | (_, _) -> 
            None

    let sourceExist = 
        match assemPath, projectPath with
        | (NonEmptyString, EmptyString) when (not (File.Exists assemPath)) -> 
            Some(sprintf "error: assembly file not found at %s" assemPath)
        | (EmptyString, NonEmptyString) when (not (File.Exists projectPath)) -> 
            Some(sprintf "error: project file not found at %s" projectPath)
        | (_, _) -> 
            None

    let outParseResult = 
        match outPath, stdOut with
        | (EmptyString, false) -> 
            Some "error: either --out-path or --std-out is required"
        | (NonEmptyString, true) -> 
            Some "error: both --out-path and --std-out specified. one or the other is required"
        | (_, _) -> 
            None

    let validationChecks = [sourceParseResult; outParseResult; sourceExist]
    let failureMessages = seq { for m in validationChecks do if m.IsSome then yield m.Value}

    let mutable exitCode = 0

    if not(Seq.isEmpty(failureMessages)) then
        for m in failureMessages do   
            printf "%s\n" m
        ArgParser.Usage(args, usage = usageText)
        exitCode <- 1
    else 
        let compileResult = 
            match assemPath, projectPath with 
            | (NonEmptyString, EmptyString) ->
                Compile.AssemblyToFunScript(Reflection.Assembly.LoadFile(assemPath))
            | (EmptyString, NonEmptyString) ->
                Compile.ProjectToFunScript(projectPath)
            | (_, _) ->
                failwithf "shouldnt ever get here"
        
        if compileResult.Success then
            if(stdOut) then
                printf "%s" compileResult.CompiledFunScript
            else
                let outFile = FileInfo(outPath)
                outFile.Directory.Create()
                File.WriteAllText(outPath, compileResult.CompiledFunScript)
        else
            exitCode <- 1
            for err in compileResult.Errors do
                printf "%s" err.Message
    exitCode