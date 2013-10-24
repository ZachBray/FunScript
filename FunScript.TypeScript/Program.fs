module FunScript.TypeScript.Provider

open System
open System.Diagnostics
open System.Reflection
open System.IO
open System.Net
open System.Net.Security
open System.CodeDom.Compiler

open Ionic.Zip

open FunScript.TypeScript.AST
open FunScript.TypeScript.Parser
open FunScript.TypeScript.TypeGenerator

let openUriStream uri =
    let acceptAllCerts = RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
    ServicePointManager.ServerCertificateValidationCallback <- acceptAllCerts            
    let req = System.Net.WebRequest.Create(Uri(uri))
    let resp = req.GetResponse() 
    resp.GetResponseStream()

/// Open a file from file system or from the web in a type provider context
/// (File may be relative to the type provider resolution folder and web
/// resources must start with 'http://' prefix)
let openFileOrUri resolutionFolder (fileName:string) =
    if fileName.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
        fileName.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase) then
        new StreamReader(openUriStream fileName)
    else
        // If the second path is absolute, Path.Combine returns it without change
        let file = 
            if fileName.StartsWith ".." then Path.Combine(resolutionFolder, fileName)
            else fileName
        new StreamReader(file)

let compile outputAssembly references source =
    use provider = new Microsoft.FSharp.Compiler.CodeDom.FSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly = outputAssembly)
    let msCorLib = typeof<int>.Assembly.Location
    parameters.ReferencedAssemblies.Add msCorLib |> ignore<int>
    let fsCorLib = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\3.0\Runtime\v4.0\FSharp.Core.dll"
    parameters.ReferencedAssemblies.Add fsCorLib |> ignore<int>
    let funScriptInterop = typeof<FunScript.JSEmitInlineAttribute>.Assembly.Location
    parameters.ReferencedAssemblies.Add funScriptInterop |> ignore<int>
    parameters.ReferencedAssemblies.AddRange references
    let sourceFile = outputAssembly + ".fs"
    File.WriteAllText(sourceFile, source)
    /// This is to get the code dom to work!
    if System.Environment.GetEnvironmentVariable("FSHARP_BIN") = null then
        let defaultFSharpBin = @"C:\Program Files (x86)\Microsoft SDKs\F#\3.0\Framework\v4.0"
        if Directory.Exists defaultFSharpBin then
            Environment.SetEnvironmentVariable("FSHARP_BIN", defaultFSharpBin)
        else failwith "Expected FSHARP_BIN environment variable to be set."
    let results = provider.CompileAssemblyFromFile(parameters, [|sourceFile|])
    match [| for err in results.Errors -> err |] with
    | [||] -> true
    | errors ->
        printf "Failed to compile. \n%A" (errors |> Array.map (fun err -> err.ErrorText))
        false

let generateAssembliesLazily tempDir inputs =
    if not(Directory.Exists tempDir) then
        Directory.CreateDirectory tempDir |> ignore
    let outputFiles =
        inputs
        |> Seq.map (fun (name, contents) -> name, Parser.parseDeclarationsFile contents)
        |> Seq.toList
        |> TypeGenerator.Compiler.generateTypes
    let assemblyLocation moduleName =
        Path.Combine(tempDir, "FunScript.TypeScript.Binding." + moduleName + ".dll")
    let outputMappings =
        outputFiles |> Seq.map (fun (name, contents, dependencies) ->
            name, (contents, dependencies))
        |> Map.ofSeq
    let rec forceCreation name =
        let contents, dependencies = outputMappings.[name]
        let moduleLocation = assemblyLocation name
        if File.Exists moduleLocation then
            Some(moduleLocation, dependencies)
        else
            printfn "Generating assembly for %s..." name
            let hasDependencies =
                dependencies |> List.forall (forceCreation >> Option.isSome)
            let dependencyLocations =
                dependencies |> List.toArray |> Array.map assemblyLocation
            let wasSuccessful =
                hasDependencies &&
                compile 
                    moduleLocation
                    dependencyLocations
                    contents
            if wasSuccessful then Some(moduleLocation, dependencies)
            else None
    outputFiles |> List.map (fun (name, _, _) ->
        name, lazy forceCreation name)
    
let loadDefaultLib() =
    let ass = typeof<AST.AmbientClassBodyElement>.Assembly
    use stream = ass.GetManifestResourceStream("lib.d.ts")
    use reader = new StreamReader(stream)
    "lib", reader.ReadToEnd()

// DefaultUri = "https://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
let downloadTypesZip tempDir zipUri =
    let tempFile = Path.Combine(tempDir, "Types.zip")
    if not (File.Exists tempFile) then
        printfn "Downloading DefinitelyTyped repository..."
        use stream = openUriStream zipUri
        use memStream = new MemoryStream()
        stream.CopyTo memStream
        File.WriteAllBytes(tempFile, memStream.ToArray())
    let zip = ZipFile.Read tempFile
    let moduleContents =
        zip.Entries 
        |> Seq.filter (fun entry ->
            entry.FileName.EndsWith ".d.ts" && not (entry.FileName.Contains "test"))
        |> Seq.map (fun entry -> 
            use stream = entry.OpenReader()
            use reader = new StreamReader(stream)
            let filename = Path.GetFileName entry.FileName
            let moduleName = filename.Substring(0, filename.Length - ".d.ts".Length)
            moduleName, reader.ReadToEnd())
        |> Seq.toList
    let hasLib = moduleContents |> Seq.exists (fst >> ((=) "lib"))
    if hasLib then moduleContents
    else loadDefaultLib() :: moduleContents

let template =
    lazy
        let ass = typeof<AST.AmbientClassBodyElement>.Assembly
        use stream = ass.GetManifestResourceStream("template.nuspec")
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

let nuspec name version dependencies =
    let deps =
        dependencies 
        |> Seq.map (fun n -> sprintf "        <dependency id=\"FunScript.TypeScript.Binding.%s\" version=\"%s\" />" n version) 
        |> String.concat System.Environment.NewLine
    template.Value
        .Replace("{package}", name)
        .Replace("{version}", version)
        .Replace("{dependencies}", deps)

let uploadPackage tempDir key version moduleName assLoc deps =
    printfn "Generating nuspec for %s..." moduleName
    let nuspecFile = Path.Combine(tempDir, sprintf "FunScript.TypeScript.Binding.%s.nuspec" moduleName)
    File.WriteAllText(nuspecFile, nuspec moduleName version deps)
    let packProcess = Process.Start("nuget.exe", sprintf "pack %s" nuspecFile)
    packProcess.WaitForExit()
    let pushProcess = 
        Process.Start(
            "nuget.exe", 
            sprintf "push FunScript.TypeScript.Binding.%s.%s.nupkg %s" moduleName version key)
    pushProcess.WaitForExit()

[<EntryPoint>]
let main args =
    try
        let tempDir = System.Environment.CurrentDirectory
        let outDir = Path.Combine(tempDir, "Output")
        let nugetKey, zipUri =
            match args with
            | [| "--version"; version; "--push"; nugetKey; zipUri |] -> Some(version, nugetKey), zipUri
            | [| zipUri |] -> None, zipUri
            | _ -> None, "https://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
        downloadTypesZip tempDir zipUri
        |> generateAssembliesLazily outDir
        |> Seq.iter (fun (moduleName, locations) -> 
            match nugetKey with
            | None -> ()
            | Some(version, key) -> 
                match locations.Value with
                | None -> ()
                | Some (assLoc, deps) ->
                    uploadPackage outDir key version moduleName assLoc deps)
        0
    with ex -> 
        printfn "[ERROR] %s" (ex.ToString())
        1