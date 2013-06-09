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

let private getGenericArgs (mb : MethodBase) =
   if mb.IsGenericMethod || mb.IsGenericMethodDefinition then 
      mb.GetGenericArguments()
   else [||]

let private genMethod (mb:MethodBase) (replacementMi:MethodBase) (vars:Var list) bodyExpr var (compiler:InternalCompiler.ICompiler) =
   //TODO: Only add type vars when they are used? 
   // We'd have to check the body for a typeof<'a>... or threading to another method though?
   // Sounds too hard for now!
   let typeSpecVars = 
      (getGenericArgs replacementMi) |> Array.map Reflection.genTypeVar |> Array.toList
   match replacementMi.GetCustomAttribute<JSEmitAttribute>() with
   | meth when meth <> Unchecked.defaultof<_> ->
      let code =
         vars 
         |> List.mapi (fun i v -> i,v)
         |> List.fold (fun (acc:string) (i,v) ->
            acc.Replace(sprintf "{%i}" i, v.Name)
            ) meth.Emit
      [ Assign(Reference var, Lambda(typeSpecVars @ vars, EmitBlock code)) ]
   | _ when mb.IsConstructor ->
      [  
         // Here we should really pass in the type context into Compile(...).
         yield Assign(Reference var, Lambda(typeSpecVars @ vars, Block(compiler.Compile ReturnStrategies.inplace bodyExpr)))
         let methods = compiler |> Objects.genInstanceMethods mb.DeclaringType
         let proto = PropertyGet(Reference var, "prototype")
         for name, lambda in methods do
            yield Assign(PropertyGet(proto, name), lambda)
      ]
   | _ -> 
      [ Assign(
         Reference var, 
         Lambda(typeSpecVars @ vars, 
            // Here we should really pass in the type context into Compile(...).
            Block(compiler.Compile ReturnStrategies.returnFrom bodyExpr))) ] 

   

let private replaceIfAvailable (compiler:InternalCompiler.ICompiler) (mb : MethodBase) callType =
   let replacementMb =
      match compiler.ReplacementFor mb callType with
      | None -> mb //GetGenericMethod()...
      | Some mi -> upcast mi
   Reflection.specializeGeneric replacementMb

let private (|CallPattern|_|) = Objects.methodCallPattern

let private createGlobalMethod name compiler mb callType =
   match replaceIfAvailable compiler mb callType with
   | (CallPattern(vars, bodyExpr) as replacementMi), specialization ->
      // TODO: We need to deal with generic type args from the type not just the method
      getGenericArgs replacementMi,
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
      let _, consRef = createGlobalMethod name compiler ci Quote.ConstructorCall
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
   | (ReflectedDefinition name as mi) ->
      match mi.IsStatic, refs with
      | false, objRef::argRefs ->
         [ yield! decls |> List.concat
           yield returnStategy.Return <| Apply(PropertyGet(objRef, mi.Name), argRefs) ]
      | true, exprs ->
         let typeArgs, methRef = createGlobalMethod name compiler mi Quote.MethodCall
         let typeArgRefs =
            Array.map2 (fun (callT : System.Type) (receiveT : System.Type) ->
               // It could be a threaded generic argument from one func to another.
               let isRealType = not callT.IsGenericParameter
               if isRealType then
                  let runtimeTypeVar = Reflection.buildRuntimeType compiler callT
                  Reference(runtimeTypeVar)
               else
                  //TODO: Here we should really have the type context passed in to
                  // this createCall function. This is a bit hacky using Global vars.
                  // It could cause problems if the type vars have the same name as real vars.
                  Reference(Reflection.genTypeVar receiveT) 
            ) (getGenericArgs mi) typeArgs
            |> Array.toList
         [ yield! decls |> List.concat
           yield returnStategy.Return <| Apply(Reference methRef, typeArgRefs @ refs) ]
      | _ -> failwith "never"
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