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

open Microsoft.CSharp
open System.CodeDom.Compiler

let compile outputAssembly references source =
    let provider = CSharpCodeProvider.CreateProvider("CSharp")
    let parameters = CompilerParameters(OutputAssembly = outputAssembly)
    parameters.ReferencedAssemblies.Add "FSharp.Core.dll" |> ignore<int>
    parameters.ReferencedAssemblies.Add "FunScript.Interop.dll" |> ignore<int>
    parameters.ReferencedAssemblies.AddRange references 
    let results = provider.CompileAssemblyFromSource(parameters, [|source|])
    match [| for err in results.Errors -> err |] with
    | [||] -> true
    | errors -> 
        File.WriteAllText(outputAssembly + ".cs", source)
        printf "Failed to compile. \n%A" (errors |> Array.map (fun err -> err.ErrorText))
        false

let executeOnText outputFolder inputs =
    let outputFiles =
        inputs
        |> Seq.map (fun (name, contents) -> name, Parser.parseDeclarationsFile contents)
        |> Seq.toList
        |> TypeGenerator.buildCode
    if not(Directory.Exists outputFolder) then
        Directory.CreateDirectory outputFolder |> ignore
//    let dir = DirectoryInfo(outputFolder)
//    let files = dir.EnumerateFiles("*.g.cs", SearchOption.TopDirectoryOnly)
//    files |> Seq.toArray |> Array.iter (fun file -> file.Delete())
    let assemblyLocation moduleName =
        Path.Combine(outputFolder, "FunScript.TypeScript.Binding." + moduleName + ".dll")
    outputFiles |> Array.fold (fun acc (name, dependencies, contents) ->
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
            executeOnDirectory 
                @"C:\TI\FunScript\FunScript.TypeScript\bin\Debug\Typings"
                @"C:\TI\FunScript\FunScript.TypeScript\bin\Debug\Typings.Compiled" 
            printfn "Expected FunScript.TypeScript.exe --download-into <folder-path> | --generate-types <folder-path> --output-folder <folder-path>"
            1
    with ex ->
        printfn "%O" ex
        1