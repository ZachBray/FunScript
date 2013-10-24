module FunScript.TypeScript.Provider

open System
open System.Reflection
open System.IO
open System.Net
open System.Net.Security
open System.CodeDom.Compiler

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
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

let generateAssembliesLazily cacheKey inputs =
    let outputFiles =
        inputs
        |> Seq.map (fun (name, contents) -> name, Parser.parseDeclarationsFile contents)
        |> Seq.toList
        |> TypeGenerator.Compiler.generateTypes
    let assemblyLocation moduleName =
        let tempDir = Path.GetTempPath()
        Path.Combine(tempDir, "FunScript.TypeScript.Binding." + moduleName + "-" + cacheKey + ".dll")
    let outputMappings =
        outputFiles |> Seq.map (fun (name, contents, dependencies) ->
            name, (contents, dependencies))
        |> Map.ofSeq
    let rec forceCreation name =
        let contents, dependencies = outputMappings.[name]
        let moduleLocation = assemblyLocation name
        if File.Exists moduleLocation then
            Some moduleLocation
        else
            printfn "Generating assembly for %s..." name
            let hasDependencies =
                dependencies |> List.forall (forceCreation >> Option.isSome)
            let wasSuccessful =
                hasDependencies &&
                compile 
                    moduleLocation
                    (dependencies |> List.toArray |> Array.map assemblyLocation) 
                    contents
            if wasSuccessful then Some moduleLocation
            else None
    outputFiles |> List.map (fun (name, _, _) ->
        name, lazy forceCreation name)
    
let loadDefaultLib() =
    let ass = typeof<AST.AmbientClassBodyElement>.Assembly
    use stream = ass.GetManifestResourceStream("lib.d.ts")
    use reader = new StreamReader(stream)
    "lib", reader.ReadToEnd()

// DefaultUri = "https://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
let downloadTypesZip cacheKey zipUri =
    let tempDir = Path.GetTempPath()
    let tempFile = Path.Combine(tempDir, sprintf "%s.zip" cacheKey)
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

[<TypeProvider>]
type public TypeScriptTypeProvider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces() 
    
    let ns = "FunScript.TypeScript"
    let ass = Assembly.GetExecutingAssembly()
    
    let providerType = 
        ProvidedTypeDefinition(ass, ns, "TypeScriptProvider", Some typeof<obj>, HideObjectMethods = true)

    let parameters = 
        [
            ProvidedStaticParameter("CacheKey", typeof<string>, "NoKey")
            ProvidedStaticParameter("TypeZipUri", typeof<string>, "https://github.com/borisyankov/DefinitelyTyped/archive/master.zip")
        ]

    let buildTypeGraph requestedName cacheKey zipUri = 
        let reqType = ProvidedTypeDefinition(ass, ns, requestedName, Some typeof<obj>)
        let modules = downloadTypesZip cacheKey zipUri
        let moduleAssemblies = generateAssembliesLazily cacheKey modules
        reqType.AddMembers(
            [
                for moduleName, assemblyLocation in moduleAssemblies do
                    let moduleType = ProvidedTypeDefinition(moduleName, Some typeof<obj>, HideObjectMethods = true)
                    moduleType.AddAssemblyTypesAsNestedTypesDelayed(fun () ->
                        match assemblyLocation.Value with
                        | None -> null
                        | Some path -> Assembly.LoadFrom path)
                    yield moduleType
            ])
        reqType
    do 
        providerType.DefineStaticParameters(parameters, fun name -> function
            | [| :? string as cacheKey; :? string as zipUri |] -> buildTypeGraph name cacheKey zipUri
            | _ -> failwith "Invalid type parameters.")
        this.RegisterRuntimeAssemblyLocationAsProbingFolder(cfg)
        this.AddNamespace(ns, [providerType])

[<TypeProviderAssembly>]
do()