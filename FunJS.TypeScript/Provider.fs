namespace FunJS.TypeScript

open AST
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open Samples.FSharp.ProvidedTypes
open System.IO
open System
open System.Net
open System.Net.Security

[<AutoOpen>]
module private Helpers =

   /// Open a file from file system or from the web in a type provider context
   /// (File may be relative to the type provider resolution folder and web
   /// resources must start with 'http://' prefix)
   let openFileOrUri resolutionFolder (fileName:string) =
      if fileName.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
         fileName.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase) then
         let acceptAllCerts = RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
         ServicePointManager.ServerCertificateValidationCallback <- acceptAllCerts            
         let req = System.Net.WebRequest.Create(Uri(fileName))
         let resp = req.GetResponse() 
         new StreamReader(resp.GetResponseStream())
      else
         // If the second path is absolute, Path.Combine returns it without change
         let file = 
            if fileName.StartsWith ".." then Path.Combine(resolutionFolder, fileName)
            else fileName
         new StreamReader(file)
       

   let generateFrom resolutionFolder typeScriptFiles root =
      let types =
         typeScriptFiles |> Seq.collect (fun typeScriptFile ->
            use reader = openFileOrUri resolutionFolder typeScriptFile
            Parser.parse reader)
         |> Seq.toList
      TypeGenerator.generate types root
      
[<TypeProvider>]
type TypeScriptProvider(cfg:TypeProviderConfig) as this =
   inherit TypeProviderForNamespaces()

   let runtimeAssembly = Assembly.LoadFrom(cfg.RuntimeAssembly)
   
   let thisAssembly = Assembly.GetExecutingAssembly()
   let rootNamespace = "FunJS.TypeScript"
   let staticParams = [
      ProvidedStaticParameter("files", typeof<string>)
   ]

   let apiType = 
      ProvidedTypeDefinition(thisAssembly, rootNamespace, "Api", Some typeof<obj>)

   do apiType.DefineStaticParameters(
         staticParameters = staticParams,
         apply = fun typeName ->
            function
            | [| :? string as typeScriptFiles |] ->
               let typeScriptFiles = 
                  typeScriptFiles.Split([| Environment.NewLine; "\n" |], StringSplitOptions.RemoveEmptyEntries)
                  |> Seq.filter (not << String.IsNullOrWhiteSpace)
                  |> Seq.map (fun str -> str.Trim())
               let rootType =
                  ProvidedTypeDefinition(
                     thisAssembly,
                     rootNamespace,
                     typeName,
                     baseType = Some typeof<obj>)
               //System.Diagnostics.Debugger.Break()
               try generateFrom cfg.ResolutionFolder typeScriptFiles rootType
               with ex -> failwithf "Failed to generate mapping: %s\n%s" ex.Message ex.StackTrace
               rootType
            | _ -> failwith "Expected a stream as an argument"
         )

   do this.AddNamespace(rootNamespace, [apiType])

[<TypeProviderAssembly>]
do ()

module AssemblyResolver =
   do System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
      let name = System.Reflection.AssemblyName(args.Name)
      let existingAssembly = 
         System.AppDomain.CurrentDomain.GetAssemblies()
         |> Seq.tryFind(fun a -> System.Reflection.AssemblyName.ReferenceMatchesDefinition(name, a.GetName()))
      match existingAssembly with
      | Some a -> a
      | None -> null
   )