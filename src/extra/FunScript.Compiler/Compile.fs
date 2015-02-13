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
    }

    [<ReflectedDefinition>]
    let compileMethod() = 
        let x = true
        x

    let AssemblyToFunScript (assembly : Assembly) =
        // Find the main method in this assembly
        // Could offer lot more flexiblity here ...
        let mainCompileExpr =
            printfn "Searching for main function..."
//            let types = Assembly.GetExecutingAssembly().GetTypes()
//            let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
//            let mains = 
//                [ for typ in types do
//                    for mi in typ.GetMethods(flags) do
//                        if mi.Name = "compileMethod" then yield mi ]
            let types = assembly.GetTypes()
            let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
            let mains = 
                [ for typ in types do
                    for mi in typ.GetMethods(flags) do
                        if mi.Name = "main" then yield mi ]
            let mainExpr = 
                match mains with
                | [it] -> Some(Expr.Call(it, []))
                //| [it] -> Expr.TryGetReflectedDefinition(it)
                | _ -> None
            mainExpr

//        let testExpr =  
//            <@@
//                let x = true
//                x
//            @@>

        match mainCompileExpr with
        | None -> 
            {Success=false;Errors=[{Message="Could not find main function in any module."}];CompiledFunScript=""}
        | _ ->
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let source = FunScript.Compiler.Compiler.Compile(mainCompileExpr.Value, [])
            printfn "Generated JavaScript in %f sec..." (float sw.ElapsedMilliseconds / 1000.0) 
            {Success=true;Errors=Seq.empty<CompileError>;CompiledFunScript=source}

    let FSharpErrorInfoPrettyMessage (error : Microsoft.FSharp.Compiler.FSharpErrorInfo) =
        sprintf "%s %i:%i\n error: %s" error.FileName error.StartLineAlternate error.EndLineAlternate error.Message

    type AssemblyRefernce = {Path:string;ReflectedAssembly:AssemblyDefinition;AssemblyName:AssemblyName}

    type ExceptionOrAssembly= E of System.Exception | A of Assembly

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

        let results : seq<ExceptionOrAssembly> = 
            TopSort(unorderedDependencies, pred, comparer)
            |> Seq.map(fun d -> 
                try
                    A(Assembly.LoadFrom(d.Path))
                with
                | ex ->
                    E(ex))
        
        results

    let ProjectToFunScript (projectPath: string) =
        let checker = FSharpChecker.Create()
        let projectOpts = checker.GetProjectOptionsFromProjectFile(projectPath)

        let dependencyPaths =
            projectOpts.OtherOptions 
            |> Seq.filter (fun x -> x.StartsWith("-r:"))
            |> Seq.map (fun x -> x.Substring(3))

        let dependencyResults = LoadDependencies(dependencyPaths)
        let loadErrors =  dependencyResults |> Seq.choose(fun x ->
            match x with
                | A _ -> None
                | E e -> Some e)
        let dependencies = dependencyResults |> Seq.choose(fun x ->
            match x with
                | E _ -> None
                | A a -> Some a)

        match loadErrors with
            |errors when Seq.length(errors) > 0 ->
                let dependencyErrors = errors |> Seq.map(fun x -> {Message=x.Message})
                {Success=false;Errors=dependencyErrors;CompiledFunScript=""}
            |_ ->
                let scs = Microsoft.FSharp.Compiler.SimpleSourceCodeServices.SimpleSourceCodeServices()
                let requiredOptions = Seq.ofList [ "--validate-type-providers" ]

                let baseOptions = Seq.ofArray projectOpts.OtherOptions

                let options = Seq.concat [baseOptions; requiredOptions] |> Seq.distinct

                let errors, exitCode, assembly = scs.CompileToDynamicAssembly(Seq.toArray options, Some(stdout, stderr))
                let errors, exitCode = scs.Compile(Seq.toArray options)
                
                let handler = System.ResolveEventHandler(fun sender ->
                    fun eventArgs ->
                        dependencies |> Seq.find(fun x ->
                            x.FullName = eventArgs.Name)
                )

                System.AppDomain.CurrentDomain.add_AssemblyResolve(handler)
//                let errors = []
//                let exitCode = 0
                let assembly = 
                    Assembly.LoadFrom("C:\\Projects\\FunScript.CommandLine\\FunScript.CommandLine.Example\\bin\\debug\\FunScript.CommandLine.Example.dll")
                    |> Some

                match exitCode with
                    | i when (i < 1) ->
                        AssemblyToFunScript(assembly.Value)
                    | _ ->
                        let compileErrors = 
                            match errors with
                                | (_) when(Seq.isEmpty errors)->
                                    seq [{Message=sprintf "Compiler exited with code %i" exitCode}]
                                | _ ->
                                    seq {for err in errors 
                                        do yield {Message=FSharpErrorInfoPrettyMessage(err)} }
                        {Success=false;Errors=compileErrors;CompiledFunScript=""}