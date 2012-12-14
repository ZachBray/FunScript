module internal FunJS.ReflectedDefinitions

open AST
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection

let private (|List|) = Option.toList

let private (|NonNull|_|) x = 
   if obj.ReferenceEquals(x, null) then None
   else Some x

let private (|ReflectedDefinition|_|) (mi:MethodBase) =
   if mi.DeclaringType.IsInterface then Some mi.Name
   else
      match Expr.TryGetReflectedDefinition mi with
      | Some _ -> Some(JavaScriptNameMapper.mapMethod mi)
      | _ -> None

let private rootInterfaceName = typeof<IJSRoot>.Name
let private mappingInterfaceName = typeof<IJSMapping>.Name
let private conserveMappingInterfaceName = typeof<IJSConservativeMapping>.Name

let rec private buildRootWalk acc (t:System.Type) =
   match t with
   | NonNull t ->
      match t.GetInterface rootInterfaceName with
      | NonNull _ -> acc
      | _ -> buildRootWalk (t.Name :: acc) t.DeclaringType
   | _ -> failwith "An IJSMapping interface had no root"

let fixOverloads (name:string) = name.Replace("'", "")

let private removePropertyPrefixes name =
   let name = fixOverloads name
   if name.StartsWith "get_" || name.StartsWith "set_" then 
      name.Substring(4), true
   else name, false

let private (|JSMapping|_|) (mi:MethodBase) = 
   let mappingInterface = 
      mi.DeclaringType.GetInterface mappingInterfaceName
   match mappingInterface with
   | NonNull _ -> 
      let name, isProperty = removePropertyPrefixes mi.Name
      let isStatic = mi.IsStatic
      if not isStatic then Some (name, isStatic, isProperty)
      else 
         let rootWalk = buildRootWalk [] mi.DeclaringType
         match rootWalk with
         | [] -> Some (name, isStatic, isProperty)
         | _ ->
            let prefix = rootWalk |> String.concat "."
            Some (sprintf "%s.%s" prefix name, isStatic, isProperty)
   | _ -> None


let private (|JSConservativeMapping|_|) (t:System.Type) =
   let interfaceDef = t.GetInterface conserveMappingInterfaceName
   let hasInterface = interfaceDef <> Unchecked.defaultof<_>
   if hasInterface then
      let rootWalk = buildRootWalk [] t
      Some(rootWalk |> String.concat ".")
   else None

let private (|JSConservativeCtorMapping|_|) (mi:MethodBase) =
   match mi.DeclaringType with
   | JSConservativeMapping name -> Some <| name
   | _  -> None

let private (|JSConservativeMethodMapping|_|) (mi:MethodBase) =
   match mi.DeclaringType with
   | JSConservativeMapping name -> Some <| name + "." + mi.Name
   | _  -> None

let private genMethod (mb:MethodBase) (replacementMi:MethodBase) (vars:Var list) bodyExpr (compiler:InternalCompiler.ICompiler) =
   let block =
      match replacementMi.GetCustomAttribute<JSEmitAttribute>() with
      | meth when meth <> Unchecked.defaultof<_> ->
         let code =
            vars 
            |> List.mapi (fun i v -> i,v)
            |> List.fold (fun (acc:string) (i,v) ->
               acc.Replace(sprintf "{%i}" i, v.Name)
               ) meth.Emit
         EmitBlock code
      | _ when mb.IsConstructor ->
         Block [  
            yield! compiler.Compile ReturnStrategies.inplace bodyExpr
            yield! compiler |> Objects.genInstanceMethods mb.DeclaringType
         ]
      | _ -> Block(compiler.Compile ReturnStrategies.returnFrom bodyExpr)
   vars, block

let private replaceIfAvailable (compiler:InternalCompiler.ICompiler) mb callType =
   match compiler.ReplacementFor mb callType with
   | None -> mb
   | Some mi -> mi :> MethodBase

let private (|CallPattern|_|) = Objects.methodCallPattern

let private createGlobalMethod compiler mb callType =
   match replaceIfAvailable compiler mb callType with
   | CallPattern(vars, bodyExpr) as replacementMi ->
      genMethod mb replacementMi vars bodyExpr compiler
   | _ -> failwithf "No reflected definition for method: %s" mb.Name

let private createConstruction
      (|Split|) 
      (returnStategy:InternalCompiler.IReturnStrategy)
      (compiler:InternalCompiler.ICompiler)
      exprs ci =
   let exprs = exprs |> List.concat
   let decls, refs = 
      exprs 
      |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
      |> List.unzip
   match ci with
   | ReflectedDefinition name ->
      let consRef = 
         compiler.DefineGlobal name (fun var -> 
            [ Assign(Reference var, Lambda <| createGlobalMethod compiler ci Quote.ConstructorCall) ]
         )
      [ yield! decls |> List.concat
        yield returnStategy.Return <| New(consRef.Name, refs) ]
   | JSConservativeCtorMapping name ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| New(name, refs) ]
   | JSMapping(name, false, false) ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| New("Object", []) ]
   | _ -> []

let private (|SpecialOp|_|) = Quote.specialOp

let private createCall 
      (|Split|) 
      (returnStategy:InternalCompiler.IReturnStrategy)
      (compiler:InternalCompiler.ICompiler)
      exprs mi =
   let exprs = exprs |> List.concat
   let decls, refs = 
      exprs 
      |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
      |> List.unzip
   match mi, refs with
   | (JSMapping(name, true, false) as mi), _ ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Apply(Reference (Var.Global(name, typeof<obj>)), refs) ]
   | SpecialOp((ReflectedDefinition name) as mi), _
   | (ReflectedDefinition name as mi), _ ->
      match mi.IsStatic, refs with
      | false, objRef::argRefs ->
         [ yield! decls |> List.concat
           yield returnStategy.Return <| Apply(PropertyGet(objRef, mi.Name), argRefs) ]
      | true, exprs ->
         let methRef = 
            compiler.DefineGlobal name (fun var -> 
               [ Assign(Reference var, Lambda <| createGlobalMethod compiler mi Quote.MethodCall) ]
            )
         [ yield! decls |> List.concat
           yield returnStategy.Return <| Apply(Reference methRef, refs) ]
      | _ -> failwith "never"
   | JSConservativeMethodMapping name, [] ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Reference (Var.Global("!!!" + name, typeof<obj>)) ]
   | JSMapping(name, true, true), [] ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Reference (Var.Global(name, typeof<obj>)) ]
   | JSMapping("Invoke", false, false), instance::refs ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Apply(instance, refs) ]
   | JSMapping(name, false, false), instance::refs ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Apply(PropertyGet(instance, name), refs) ]
   | JSMapping(name, false, true), instance::[] ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| PropertyGet(instance, name) ]
   | _ -> []

let private methodCalling =
   CompilerComponent.create <| fun split compiler returnStategy ->
      function
      | Patterns.Call(List objExpr, mi, exprs) -> 
         createCall split returnStategy compiler [objExpr; exprs] mi
      | _ -> []

let private getPropertyField split (compiler:InternalCompiler.ICompiler) (pi:PropertyInfo) objExpr exprs =
   let name = 
      JavaScriptNameMapper.sanitize 
         (JavaScriptNameMapper.mapType pi.DeclaringType + "_" + pi.Name)
   compiler.DefineGlobal name (fun var ->
      // TODO: wrap in function scope?
      compiler.DefineGlobalInitialization <|
         createCall split (ReturnStrategies.assignVar var) compiler [objExpr; exprs] (pi.GetGetMethod(true))
      []
   )

let private propertyGetting =
   CompilerComponent.create <| fun split compiler returnStategy ->
      function
      | Patterns.PropertyGet(List objExpr, pi, exprs) ->
         let mapping = pi.GetCustomAttribute<CompilationMappingAttribute>()
         let isField = 
            mapping <> Unchecked.defaultof<_> &&
            mapping.SourceConstructFlags = SourceConstructFlags.Value
         match isField, objExpr, exprs with
         | true, [], [] ->
            let property = getPropertyField split compiler pi objExpr exprs
            [ returnStategy.Return <| Reference property ]
         | _ -> createCall split returnStategy compiler [objExpr; exprs] (pi.GetGetMethod(true))
      | _ -> []
      
let private propertySetting =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.PropertySet(List objExpr, pi, exprs, valExpr) ->
         let mapping = pi.GetCustomAttribute<CompilationMappingAttribute>()
         let isField = 
            mapping <> Unchecked.defaultof<_> &&
            mapping.SourceConstructFlags = SourceConstructFlags.Value
         match isField, objExpr, exprs, valExpr with
         | true, [], [], Split(valDecl, valRef) ->
            let property = getPropertyField (|Split|) compiler pi objExpr exprs
            [  yield! valDecl
               yield Assign(Reference property, valRef) 
            ]
         | _ -> createCall (|Split|) returnStategy compiler [objExpr; exprs; [valExpr]] (pi.GetSetMethod(true))
      | _ -> []

let private fieldGetting =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.FieldGet(Some(Split(objDecl, objRef)), fi) ->
         [ yield! objDecl
           yield returnStategy.Return <| PropertyGet(objRef, JavaScriptNameMapper.sanitize fi.Name) ]
      | Patterns.FieldGet(None, fi) ->
         let name = JavaScriptNameMapper.mapType fi.DeclaringType
         [ yield returnStategy.Return <| PropertyGet(Reference (Var.Global(name, typeof<obj>)), JavaScriptNameMapper.sanitize fi.Name) ]
      | _ -> []

let private fieldSetting =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.FieldSet(Some(Split(objDecl, objRef)), fi, Split(valDecl, valRef)) ->
         [ yield! objDecl
           yield! valDecl
           yield Assign(PropertyGet(objRef, JavaScriptNameMapper.sanitize fi.Name), valRef) ]
      | Patterns.FieldSet(None, fi, Split(valDecl, valRef)) ->
         let name = JavaScriptNameMapper.mapType fi.DeclaringType
         [ yield! valDecl
           yield Assign(PropertyGet(Reference (Var.Global(name, typeof<obj>)), JavaScriptNameMapper.sanitize fi.Name), valRef) ]
      | _ -> []

let private objectGuid = typeof<obj>.GUID

let private constructingInstances =
   CompilerComponent.create <| fun split compiler returnStategy ->
      function
      | Patterns.NewObject(ci, exprs) -> 
         if ci.DeclaringType.GUID = objectGuid then
            [ Scope <| Block [] ]
         else
            createConstruction split returnStategy compiler [exprs] ci
      | _ -> []

let components = [ 
   methodCalling
   propertyGetting
   propertySetting
   constructingInstances
   fieldGetting
   fieldSetting
]