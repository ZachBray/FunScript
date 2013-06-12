module internal FunScript.ReflectedDefinitions

open AST
open Quote
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

let private genMethod (mb:MethodBase) (replacementMi:MethodBase) (vars:Var list) bodyExpr var (compiler:InternalCompiler.ICompiler) =
   match replacementMi.GetCustomAttribute<JSEmitAttribute>() with
   | meth when meth <> Unchecked.defaultof<_> ->
      let code =
         vars 
         |> List.mapi (fun i v -> i,v)
         |> List.fold (fun (acc:string) (i,v) ->
            acc.Replace(sprintf "{%i}" i, v.Name)
            ) meth.Emit
      [ Assign(Reference var, Lambda(vars, EmitBlock code)) ]
   | _ when mb.IsConstructor ->
      [  
         yield Assign(Reference var, Lambda(vars, Block(compiler.Compile ReturnStrategies.inplace bodyExpr)))
      ]
   | _ -> 
      [ Assign(
         Reference var, 
         Lambda(vars, 
            Block(compiler.Compile ReturnStrategies.returnFrom bodyExpr))) ] 

let private (|CallPattern|_|) = Objects.methodCallPattern

let private createGlobalMethod name compiler mb callType =
   match Objects.replaceIfAvailable compiler mb callType with
   | (CallPattern(vars, bodyExpr) as replacementMi) ->
      let typeArgs = Reflection.getGenericMethodArgs replacementMi
      let specialization = Reflection.getSpecializationString compiler typeArgs
      compiler.DefineGlobal (name + specialization) (fun var ->
         genMethod mb replacementMi vars bodyExpr var compiler)
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
      //TODO: Generic types will have typeArgs we need to deal with here.
      let typeArgs = Reflection.getGenericMethodArgs ci
      let specialization = Reflection.getSpecializationString compiler typeArgs
      let consRef = createGlobalMethod (name + specialization) compiler ci Quote.ConstructorCall
      [ yield! decls |> List.concat
        yield returnStategy.Return <| New(consRef.Name, refs) ]
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
   match mi with
   | SpecialOp((ReflectedDefinition name) as mi)
   | (ReflectedDefinition name as mi) when mi.DeclaringType.IsInterface ->
      match refs with
      | [] -> []
      | objRef::argRefs ->
         [  yield! decls |> List.concat
            yield returnStategy.Return <| Apply(PropertyGet(objRef, name), argRefs) ]
   | SpecialOp((ReflectedDefinition name) as mi)
   | (ReflectedDefinition name as mi) ->
      // TODO: What about interfaces!
      let typeArgs = Reflection.getGenericMethodArgs mi
      let specialization = Reflection.getSpecializationString compiler typeArgs
      let methRef = createGlobalMethod (name + specialization) compiler mi Quote.MethodCall
      [  yield! decls |> List.concat
         yield returnStategy.Return <| Apply(Reference methRef, refs) ]
   | _ -> []

let private methodCalling =
   CompilerComponent.create <| fun split compiler returnStategy ->
      function
      | Patterns.Call(List objExpr, mi, exprs) -> 
         createCall split returnStategy compiler [objExpr; exprs] mi
      | _ -> []

let private getPropertyField split (compiler:InternalCompiler.ICompiler) (pi:PropertyInfo) objExpr exprs =
   let specialization = Reflection.getSpecializationString compiler (Reflection.getGenericTypeArgs pi.DeclaringType)
   let name = 
      JavaScriptNameMapper.sanitizeAux
         (JavaScriptNameMapper.mapType pi.DeclaringType + "_" + pi.Name + specialization)
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
           yield returnStategy.Return <| PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux fi.Name) ]
      | Patterns.FieldGet(None, fi) ->
         let name = JavaScriptNameMapper.mapType fi.DeclaringType
         [ yield returnStategy.Return <| PropertyGet(Reference (Var.Global(name, typeof<obj>)), JavaScriptNameMapper.sanitizeAux fi.Name) ]
      | _ -> []

let private fieldSetting =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.FieldSet(Some(Split(objDecl, objRef)), fi, Split(valDecl, valRef)) ->
         [ yield! objDecl
           yield! valDecl
           yield Assign(PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux fi.Name), valRef) ]
      | Patterns.FieldSet(None, fi, Split(valDecl, valRef)) ->
         let name = JavaScriptNameMapper.mapType fi.DeclaringType
         [ yield! valDecl
           yield Assign(PropertyGet(Reference (Var.Global(name, typeof<obj>)), JavaScriptNameMapper.sanitizeAux fi.Name), valRef) ]
      | _ -> []

let private objectGuid = typeof<obj>.GUID
let private objectName = typeof<obj>.FullName

let private constructingInstances =
   CompilerComponent.create <| fun split compiler returnStategy ->
      function
      | PatternsExt.NewObject(ci, exprs) -> 
         let declaringType = ci.DeclaringType
         if declaringType.GUID = objectGuid &&
            declaringType.FullName = objectName 
         then [ Scope <| Block [] ]
         else createConstruction split returnStategy compiler [exprs] ci
      | _ -> []

let components = [ 
   methodCalling
   propertyGetting
   propertySetting
   constructingInstances
   fieldGetting
   fieldSetting
]