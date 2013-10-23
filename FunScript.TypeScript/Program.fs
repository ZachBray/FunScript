module FunScript.TypeScript.Program

open System
open System.IO
open System.Net
open System.Net.Security

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

open System.CodeDom.Compiler

let compile outputAssembly references source =
    use provider = new Microsoft.FSharp.Compiler.CodeDom.FSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly = outputAssembly)
    let addLocalReference path = 
        let currentDirectory = Environment.CurrentDirectory
        let fullPath = Path.Combine(currentDirectory, path)
        parameters.ReferencedAssemblies.Add fullPath |> ignore<int>
    let msCorLib = typeof<int>.Assembly.Location
    parameters.ReferencedAssemblies.Add msCorLib |> ignore<int>
    let fsCorLib = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\3.0\Runtime\v4.0\FSharp.Core.dll"
    parameters.ReferencedAssemblies.Add fsCorLib |> ignore<int>
    addLocalReference "FunScript.Interop.dll"
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

let executeOnText outputFolder inputs =
    let outputFiles =
        inputs
        |> Seq.map (fun (name, contents) -> name, Parser.parseDeclarationsFile contents)
        |> Seq.toList
        |> TypeGenerator.Compiler.generateTypes
    if not(Directory.Exists outputFolder) then
        Directory.CreateDirectory outputFolder |> ignore
    let assemblyLocation moduleName =
        Path.Combine(outputFolder, "FunScript.TypeScript.Binding." + moduleName + ".dll")
    outputFiles |> List.fold (fun acc (name, contents, dependencies) ->
        printfn "Generating assembly for %s..." name
        let hasDependencies =
            dependencies |> List.forall (fun x -> acc |> Set.contains x)
        let wasSuccessful =
            hasDependencies &&
            compile 
                (assemblyLocation name) 
                (dependencies |> List.toArray |> Array.map assemblyLocation) 
                contents
        if wasSuccessful then acc |> Set.add name
        else acc) Set.empty
    |> ignore

let executeOnDirectory inputFolder outputFolder =
    let dir = DirectoryInfo(inputFolder)
    let files = dir.EnumerateFiles("*.d.ts", SearchOption.AllDirectories)
    files |> Seq.map (fun file -> 
        file.Name.Substring(0, file.Name.Length - ".d.ts".Length), 
        file.OpenText().ReadToEnd())
    |> executeOnText outputFolder
    
let download (baseDirectory : string) =
    if not (Directory.Exists baseDirectory) then
        Directory.CreateDirectory baseDirectory |> ignore
    printfn "Downloading DefinitelyTyped repository..."
    use stream = openUriStream "https://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
    use memStream = new MemoryStream()
    stream.CopyTo memStream
    let typesFile = System.Environment.CurrentDirectory + @"\Types.zip"
    File.WriteAllBytes(typesFile, memStream.ToArray())
    let zip = ZipFile.Read typesFile
    zip.Entries 
    |> Seq.filter (fun entry ->
        entry.FileName.EndsWith ".d.ts" && not (entry.FileName.Contains "test"))
    |> Seq.iter (fun entry -> 
        use stream = entry.OpenReader()
        use reader = new StreamReader(stream)
        let path = Path.Combine(baseDirectory, Path.GetFileName entry.FileName)
        File.WriteAllText(path, reader.ReadToEnd()))

[<EntryPoint>]
let main(args : string[]) =
    try
        match args with
        | [| "--download-into"; outputFolder |] ->
            download outputFolder
            0
        | [| "--generate-types"; inputFolder; "--output-folder"; outputFolder |] ->
            executeOnDirectory inputFolder outputFolder
            0
        | _ ->  
            #if DEBUG
            executeOnDirectory 
                @"C:\TI\FunScript\FunScript.TypeScript\bin\Debug\Typings"
                @"C:\TI\FunScript\FunScript.TypeScript\bin\Debug\Typings.Compiled"
            System.Console.ReadLine() |> ignore
            0
            #else
            printfn "Expected FunScript.TypeScript.exe --download-into <folder-path> | --generate-types <folder-path> --output-folder <folder-path>"
            1
            #endif
    with ex ->
        printfn "%O" ex
        1