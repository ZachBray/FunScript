module Compile

    open System.Reflection
    open System.Collections.Generic
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Mono.Cecil

    type CompileError = {Message:string}
    type CompileResult = {
        Success: bool
        Errors: seq<CompileError>
        CompiledFunScript: string
    } with
        static member MakeSingleError message =
            {Success=false;Errors=[{Message=message}];CompiledFunScript=""}
        static member MakeSuccess source = 
            {Success=true;Errors=Seq.empty<CompileError>;CompiledFunScript=source}

    [<ReflectedDefinition>]
    let compileMethod() = 
        let x = true
        x

    type ExceptionOrT<'t> =
    | Exception of System.Exception
    | T of 't

    let AssemblyToFunScript (assembly : Assembly) (funScriptAssembly: Assembly) : CompileResult  =
        //let funScriptAssembly = Assembly.Load("FunScript")
        let compilerTypeName = "FunScript.Compiler+Compiler"
        let compileMethodName = "CompileAssembly"//"Compile"

        let getFunScriptType fullName = 
            funScriptAssembly.GetTypes() 
            |> Seq.tryFind(fun x -> x.FullName = fullName)

        let funScriptCompiler = getFunScriptType compilerTypeName

        match funScriptCompiler with
        | None ->
            CompileResult.MakeSingleError(sprintf "Could not find type \"%s\" in FunScript assembly" compilerTypeName)
        | Some funScriptCompiler ->
            let compileMethod = 
                funScriptCompiler.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                |> Seq.tryFind(fun x -> x.Name = compileMethodName)
            match compileMethod with
            | None ->
                CompileResult.MakeSingleError(sprintf "Could not find method \"%s\" on type \"%s\" in FunScript assembly" compileMethodName compilerTypeName)
            | Some compileMethod ->
                let sw = System.Diagnostics.Stopwatch.StartNew()
             
                let invokeParams : array<obj> = [| assembly; None; None; None |]

                let source : ExceptionOrT<string> = 
                    try
                        T(compileMethod.Invoke(null, invokeParams) :?> string)
                    with ex ->
                        Exception(ex)

                match source with
                | Exception ex ->
                    CompileResult.MakeSingleError(sprintf "Exception during compile:\n%s" "" )//ex.ToString()
                | T source ->
                    CompileResult.MakeSuccess(source)

    let FSharpErrorInfoPrettyMessage (error : Microsoft.FSharp.Compiler.FSharpErrorInfo) =
        sprintf "%s %i:%i\n error: %s" error.FileName error.StartLineAlternate error.EndLineAlternate error.Message

    type AssemblyRefernce = {Path:string;ReflectedAssembly:AssemblyDefinition;AssemblyName:AssemblyName}

    let LoadDependencies (dependencyPaths : seq<string>) = 
       
        let unorderedDependencies =
            dependencyPaths
            |> Seq.map(fun path -> 
                let assem = AssemblyDefinition.ReadAssembly(path)
                {Path=path;ReflectedAssembly=assem;AssemblyName=AssemblyName(assem.FullName)}
            )

        let TopSort(roots: seq<'T>, pred: 'T -> seq<'T>, nodeIdentity: IEqualityComparer<'T>) =
            seq {
                let visited = HashSet(nodeIdentity)
                let rec visit node =
                    seq {
                        if visited.Add(node) then
                            for pN in pred node do
                                yield! visit pN
                            yield node
                    }
                for node in roots do
                    yield! visit node
            }

        let comparer =
                HashIdentity.FromFunctions<AssemblyRefernce>
                    (fun a -> hash a.Path)
                    (fun a b -> a.Path = b.Path)

        let pred (a: AssemblyRefernce) =
            a.ReflectedAssembly.MainModule.AssemblyReferences
            |> Seq.choose (fun r ->
                let n = AssemblyName(r.FullName)
                match unorderedDependencies |> Seq.tryFind(fun x -> x.AssemblyName.FullName = n.FullName) with
                | None -> None
                | Some assemRef ->
                    assemRef
                    |> Some)

        (*
        Seperate compile domain support.
        let handler = ResolveEventHandler(fun sender ->
            fun eventArgs ->
                null)
        compileDomain.add_AssemblyResolve(handler)
        *)

        let results : seq<ExceptionOrT<Assembly>> = 
            TopSort(unorderedDependencies, pred, comparer)
            |> Seq.map(fun d -> 
                try
                    T(Assembly.LoadFrom(d.Path))
                with
                | ex ->
                    Exception(ex))
        
        results

    let ProjectToFunScript (projectPath: string) : CompileResult =
        let checker = FSharpChecker.Create()
        let projectOpts = checker.GetProjectOptionsFromProjectFile(projectPath)

        let dependencyPaths =
            projectOpts.OtherOptions 
            |> Seq.filter (fun x -> x.StartsWith("-r:"))
            |> Seq.map (fun x -> x.Substring(3))

        let dependencyResults = LoadDependencies(dependencyPaths)
        let loadErrors =  dependencyResults |> Seq.choose(fun x ->
            match x with
                | T _ -> None
                | Exception e -> Some e)
        let dependencies = dependencyResults |> Seq.choose(fun x ->
            match x with
                | Exception _ -> None
                | T a -> Some a)

        match loadErrors with
            |errors when Seq.length(errors) > 0 ->
                let dependencyErrors = errors |> Seq.map(fun x -> {Message=x.Message})
                {Success=false;Errors=dependencyErrors;CompiledFunScript=""}
            |_ ->
                let scs = Microsoft.FSharp.Compiler.SimpleSourceCodeServices.SimpleSourceCodeServices()
                let requiredOptions = Seq.ofList [ "--validate-type-providers" ]

                let baseOptions = Seq.ofArray projectOpts.OtherOptions
                
                let outParam = "--out:"
                let outAssemblyPath = 
                    baseOptions 
                    |> Seq.find(fun x -> x.StartsWith outParam)

                let outAssemblyPath = outAssemblyPath.Substring(outParam.Length)

                let options = Seq.concat [baseOptions; requiredOptions] |> Seq.distinct

                //let errors, exitCode, assembly = scs.CompileToDynamicAssembly(Seq.toArray options, Some(stdout, stderr))
                let errors, exitCode = scs.Compile(Seq.toArray options)
                
                let handler = System.ResolveEventHandler(fun sender ->
                    fun eventArgs ->
                        dependencies |> Seq.find(fun x ->
                            x.FullName = eventArgs.Name)
                )

                System.AppDomain.CurrentDomain.add_AssemblyResolve(handler)
                let outAssembly = 
                    Assembly.LoadFrom(outAssemblyPath)
                    |> Some

                match exitCode with
                    | i when (i < 1) ->
                        match dependencies |> Seq.tryFind(fun x -> x.GetName().Name = "FunScript") with 
                        | None -> 
                            {Success=false;Errors=[{Message="Project must reference FunScript"}];CompiledFunScript=""}
                        | Some funScriptAssembly ->
                            AssemblyToFunScript outAssembly.Value funScriptAssembly
                    | _ ->
                        let compileErrors = 
                            match errors with
                                | (_) when(Seq.isEmpty errors)->
                                    seq [{Message=sprintf "Compiler exited with code %i" exitCode}]
                                | _ ->
                                    seq {for err in errors 
                                        do yield {Message=FSharpErrorInfoPrettyMessage(err)} }
                        {Success=false;Errors=compileErrors;CompiledFunScript=""}