namespace FunJS.TypeScript

open AST
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open Samples.FSharp.ProvidedTypes
open System.IO
open System



module TypeGenerator =

   let makeTupleType ts =
      let tupleType =
         match ts |> List.length with
         | 2 -> typedefof<_ * _>
         | 3 -> typedefof<_ * _ * _>
         | 4 -> typedefof<_ * _ * _ * _>
         | 5 -> typedefof<_ * _ * _ * _ * _>
         | 6 -> typedefof<_ * _ * _ * _ * _ * _>
         | 7 -> typedefof<_ * _ * _ * _ * _ * _ * _>
         | _ -> failwith "Unsupported tuple size"
      ProvidedSymbolType(Generic tupleType, ts) :> Type

   let rec getActualType obtainDef = function
      | Any -> typeof<obj>
      | Boolean -> typeof<bool>
      | String -> typeof<string>
      | Number -> typeof<float>
      | Void -> typeof<unit>
      | TSType.Array (TSType.Array t) ->
         let actualT: System.Type = getActualType obtainDef t
         try actualT.MakeArrayType(2)
         with _ -> failwithf "Could not make array from Type: %A" t
      | TSType.Array t -> 
         let actualT: System.Type = getActualType obtainDef t
         try actualT.MakeArrayType()
         with _ -> failwithf "Could not make array from Type: %A" t
      //TODO: Param arrays
      | Lambda([], retT) ->
         //let domain = typeof<unit>
         let range = getActualType obtainDef retT
         // NOTE:
         // You would think that it would be ok to do this here:
         //    FSharpType.MakeFunctionType(domain, range)
         // It isn't because if the domain/range includes a ProvidedType
         // (which is _not_ a runtime type) it creates a TypeBuilderInstance
         // rather than a runtime type. TypeBuilderInstance does not implement
         // IsAssignableFrom. This means that calls to the method that takes
         // or returns the lambda fail the checkArgs test when creating a Call 
         // quotation Expr.
         let genericType = typedefof<_ -> _>
         ProvidedSymbolType(SymbolKind.Generic genericType, [typeof<unit>; range]) :> Type
      | Lambda([p], retT) ->
         let domain = getActualType obtainDef p.Var.Type
         let range = getActualType obtainDef retT
         let genericType = typedefof<_ -> _>
         ProvidedSymbolType(SymbolKind.Generic genericType, [domain; range]) :> Type
      | Lambda(ps, retT) ->
         try
            let domain = 
               ps 
               |> List.map (fun p -> getActualType obtainDef p.Var.Type)
               |> makeTupleType
            let range = getActualType obtainDef retT
            let genericType = typedefof<_ -> _>
            ProvidedSymbolType(SymbolKind.Generic genericType, [domain; range]) :> Type
         with ex ->
            failwithf "Failed to make function type. %s For: %A => %A" ex.Message ps retT
      | t -> obtainDef t :> System.Type   

   type MemberType = Global | Local

   let genProperty obtainDef memType (var:TSVariable) =
      let name = var.Name
      let prop =
         ProvidedProperty(
            name, 
            getActualType obtainDef var.Type, 
            [])
      prop.IsStatic <- (memType = Global)
      prop.SetterCode <- fun _ -> <@@ failwithf "" @@>
      prop.GetterCode <- fun _ -> <@@ failwithf "" @@>
      prop
      
   let genParameter obtainDef (p:TSParameter) =
      let parameter =
         ProvidedParameter(
            p.Var.Name, 
            getActualType obtainDef p.Var.Type,
            false,
            p.Var.IsOptional)
      //parameter.IsParamArray <- p.IsParamArray
      parameter

   let generated = Dictionary<ProvidedTypeDefinition, Set<name * TSType list> ref>()

   let getGeneratedSet t =
      match generated.TryGetValue t with
      | false, _ ->
         let genSet = ref Set.empty
         generated.Add(t, genSet)
         genSet
      | true, genSet -> genSet

   let genCons t obtainDef memType (f:TSFunction) =
      let parameters = 
         f.Parameters |> List.map (genParameter obtainDef)
      ProvidedConstructor(parameters, InvokeCode = fun _ -> <@@ failwithf "" @@>)

// Note: We could move to something like this here where all permutations of optional 
//       arguments are allowed. At the moment it is all or none (in terms of optional params).
//       However, this would be rubbish with the current overloading problems because
//       the method names would be ridiculous. E.g., "Foo''''''''''''()".
//   let rec createOptionalPermutations parameters =
//      seq {
//         match parameters with
//         | [] -> yield []
//         | (p:TSParameter)::ps ->
//            for ps in createOptionalPermutations ps do
//               if p.Var.IsOptional then
//                  yield ps
//               yield p::ps
//      }

   let genMethods (t:ProvidedTypeDefinition) obtainDef memType (f:TSFunction) =
      let name =
         match f.Name with
         | "" -> "Invoke"
         | name -> name
      let createMethod parameters =
         let parameters = 
            parameters |> List.map (genParameter obtainDef)
         let retType =
            match f.Type with
            | Void -> typeof<System.Void>
            | _ -> getActualType obtainDef f.Type
         let rec safeName suggestion = 
            if t.GetMember(suggestion) |> Array.isEmpty then suggestion
            else safeName (suggestion + "'")
         let meth =
            ProvidedMethod(
               safeName name,
               parameters,
               retType)
         match memType with
         | Global -> meth.IsStaticMethod <- true
         | Local -> meth.AddMethodAttrs MethodAttributes.Virtual
         meth.InvokeCode <- fun _ -> <@@ failwith "never" @@>
         meth
     
      let genSet = getGeneratedSet t
      let allParamTypes = f.Parameters |> List.map (fun p -> p.Var.Type)         
      if not(!genSet |> Set.contains (name, allParamTypes)) then
         genSet := !genSet |> Set.add (name, allParamTypes)
         t.AddMember <| createMethod f.Parameters
      let reqParams = f.Parameters |> List.filter (fun p -> not p.Var.IsOptional)
      let reqParamTypes = reqParams |> List.map (fun p -> p.Var.Type)
      if not(!genSet |> Set.contains (name, reqParamTypes)) then
         genSet := !genSet |> Set.add (name, reqParamTypes)
         t.AddMember <| createMethod reqParams
         
   let genEnum (enumT:ProvidedTypeDefinition) caseNames =
      let rec addCases = function
         | [] -> ()
         | caseName::rest ->
            let prop = ProvidedProperty(caseName, enumT, [])
            prop.GetterCode <- fun _ -> <@@ failwith "never" @@>
            prop.IsStatic <- true
            enumT.AddMember prop
            addCases rest
      addCases caseNames
      
   let rec addMembers (root:ProvidedTypeDefinition) obtainDef memType remaining =
      match remaining with
      | [] -> ()
      | curr :: rest ->
         let inline next() = addMembers root obtainDef memType rest
         match curr with
         | Property v -> 
            let property = genProperty obtainDef memType v 
            root.AddMember property
            next()
         | Method f ->
            genMethods root obtainDef memType f
            next()
         | _ -> 
            // TODO: indexers
            next()

   let rec addTypes (root:ProvidedTypeDefinition) obtainDef remaining =
      match remaining with
      | [] -> ()
      | curr :: rest ->
         let inline next() = addTypes root obtainDef rest
         match curr with
         | DeclareVar v ->
            let property = genProperty (obtainDef root) Global v 
            root.AddMember property
            next()  
         | DeclareFunction f ->
            genMethods root (obtainDef root) Global f
            next()
         | DeclareEnum(name, names) ->
            let t = obtainDef root (Enumeration name)
            genEnum t names
            t.AddInterfaceImplementation typeof<FunJS.IJSConservativeMapping>
            next()
         | DeclareObject o ->
            let tsT = GlobalObject o.Name
            let t = obtainDef root tsT
            addMembers t (obtainDef root) Global o.Members
            next()
         | DeclareInterface o ->
            let tsT = Interface o.Name
            let t = obtainDef root tsT
            addMembers t (obtainDef root) Local o.Members            
            next()
         | DeclareClass o ->
            let tsT = Class o.Name
            let t = obtainDef root tsT
            t.AddInterfaceImplementation typeof<FunJS.IJSConservativeMapping>
            let constructors, members = 
               o.Members |> List.partition (function
                  | Property _ | Indexer _ -> false
                  | Method f -> f.Name = "constructor")
            match constructors with
            | [] ->
               let cons = ProvidedConstructor []
               cons.InvokeCode <- fun _ -> <@@ failwithf "" @@>
               t.AddMember cons
            | _ -> 
               let bestCons = 
                  constructors
                  |> List.choose (function
                     | Method f -> Some f
                     | _ -> None)
                  |> List.maxBy (fun f -> f.Parameters.Length)
               let cons = genCons t (obtainDef root) Local bestCons
               t.AddMember cons
            addMembers t (obtainDef root) Local members        
            next()
         | DeclareModule(typePath, declarations) ->
            let path = typePath.Split '.' |> Array.toList
            match path with
            | [] -> failwith "never"
            | name::[] -> 
               let t = obtainDef root (GlobalObject name)
               addTypesSorted t obtainDef declarations
            | name::names ->
               let t = obtainDef root (GlobalObject name)
               let path = names |> String.concat "."
               addTypes t obtainDef [DeclareModule(path, declarations)]
            next()

   and addTypesSorted root obtainDef declarations =
      // We need to pre-register all Enums and Classes here to avoid
      // them getting funny primed names like the interfaces might.
      declarations |> List.choose (function
         | DeclareEnum(name, _) -> Some(Enumeration name)
         | DeclareClass c -> Some(Class c.Name)
         | _ -> None)
      |> List.iter (fun t -> obtainDef root t |> ignore)
      addTypes root obtainDef declarations

   /// Open a file from file system or from the web in a type provider context
   /// (File may be relative to the type provider resolution folder and web
   /// resources must start with 'http://' prefix)
   let openFileOrUri resolutionFolder (fileName:string) =
      if fileName.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) then
         let req = System.Net.WebRequest.Create(Uri(fileName))
         let resp = req.GetResponse() 
         new StreamReader(resp.GetResponseStream())
      else
         // If the second path is absolute, Path.Combine returns it without change
         let file = Path.Combine(resolutionFolder, fileName)
         new StreamReader(file)
       

   let generateFrom (resolutionFolder:string) (typeScriptFile:string) (root:ProvidedTypeDefinition) =
      let created = Dictionary()
      let count = ref 0

      let rec makeType parentT = function
         | GlobalObject name | Enumeration name | Class name ->
            let actualName = name.Split '.' |> Seq.last
            ProvidedTypeDefinition(name, None)
         | Interface name ->
            let actualName = name.Split '.' |> Seq.last
            ProvidedTypeDefinition(name + "'", None)
         | Structural ps -> 
            incr count
            let t = ProvidedTypeDefinition(sprintf "TempType%i" !count, None)
            addMembers t (obtainTypeDef parentT) Local ps
            t
         | _ -> failwith "not implemented"

      and obtainTypeDef (parentT:ProvidedTypeDefinition) tsT =
         match created.TryGetValue tsT with
         | true, t -> t
         | false, _ -> 
            let t = makeType parentT tsT
            t.IsErased <- false
            match tsT with
            | Class _ -> ()
            | _ ->
               let cons = ProvidedConstructor []
               cons.InvokeCode <- fun _ -> <@@ failwithf "" @@>
               t.AddMember cons
            t.AddInterfaceImplementation typeof<FunJS.IJSMapping>
            created.Add(tsT, t)
            // Enums can be referenced in other types but their
            // names cannot be changed or it will break the FunScript
            // integration.
            match tsT with
            | Enumeration name | Class name -> created.Add(Interface name, t)
            | _ -> () 
            parentT.AddMember t
            t
      
      use reader = openFileOrUri resolutionFolder typeScriptFile
      let types = Parser.parse reader
      reader.Close()             
      addTypesSorted root obtainTypeDef types
      
[<TypeProvider>]
type TypeScriptProvider(cfg:TypeProviderConfig) as this =
   inherit TypeProviderForNamespaces()

   let thisAssembly = Assembly.GetExecutingAssembly()
   let rootNamespace = "FunJS.TypeScript"
   let staticParams = [
      ProvidedStaticParameter("file", typeof<string>)
   ]

   let apiType = 
      ProvidedTypeDefinition(thisAssembly, rootNamespace, "Api", None)

   do apiType.IsErased <- false
   do apiType.DefineStaticParameters(
         staticParameters = staticParams,
         apply = fun typeName ->
            function
            | [| :? string as typeScriptFile |] ->
               let rootType =
                  ProvidedTypeDefinition(
                     thisAssembly,
                     rootNamespace,
                     typeName,
                     baseType = None)
               rootType.IsErased <- false
               rootType.AddInterfaceImplementation typeof<FunJS.IJSRoot>
               rootType.AddInterfaceImplementation typeof<FunJS.IJSMapping>
               //System.Diagnostics.Debugger.Break()
               try TypeGenerator.generateFrom cfg.ResolutionFolder typeScriptFile rootType
               with ex -> failwithf "Failed to generate TypeScript mapping: %s\n%s" ex.Message ex.StackTrace
               let path = System.IO.Path.GetTempFileName() + ".dll"
               rootType.ConvertToGenerated path
               rootType
            | _ -> failwith "Expected a stream as an argument"
         )

   do this.AddNamespace(rootNamespace, [apiType])

[<TypeProviderAssembly>]
do ()


