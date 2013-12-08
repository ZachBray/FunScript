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
      match Expr.tryGetReflectedDefinition mi with
      | Some _ -> Some(JavaScriptNameMapper.mapMethod mi)
      | _ -> None

let private genMethod (mb:MethodBase) (replacementMi:MethodBase) (vars:Var list) bodyExpr var (compiler:InternalCompiler.ICompiler) =
   match replacementMi.GetCustomAttribute<JSEmitAttribute>() with
   | meth when meth <> Unchecked.defaultof<_> ->
      let code padding (scope : VariableScope ref) =
         vars 
         |> List.mapi (fun i v -> i,v)
         |> List.fold (fun (acc:string) (i,v) ->
            acc.Replace(sprintf "{%i}" i, (Reference v).Print(padding, scope))
            ) meth.Emit
      [ Assign(Reference var, Lambda(vars, Block[EmitStatement(fun (padding, scope) -> code padding scope)])) ]
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
   | (CallPattern getVarsExpr as replacementMi) ->
      let typeArgs = Reflection.getGenericMethodArgs replacementMi
      let specialization = Reflection.getSpecializationString compiler typeArgs
      compiler.DefineGlobal (name + specialization) (fun var ->
         let vars, bodyExpr = getVarsExpr()
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
      let consRef = createGlobalMethod name compiler ci Quote.ConstructorCall
      [ yield! decls |> List.concat
        yield returnStategy.Return <| New(consRef, refs) ]
   | _ -> []


let (|OptionPattern|_|) = function
    | Patterns.NewUnionCase(uci, []) when 
            uci.Tag = 0 && 
            uci.DeclaringType.Name = typeof<obj option>.Name ->
        Some(None)
    | Patterns.NewUnionCase(uci, [expr]) when 
            uci.Tag = 1 && 
            uci.DeclaringType.Name = typeof<obj option>.Name ->
        Some(Some expr)
    | _ -> None

let tryReplace (x : string) y (str : string) =
    let newStr = str.Replace(x, y)
    if str <> newStr then Some newStr
    else None

let orElse f = function
    | None -> f()
    | Some _ as x -> x

let private createEmitInlineExpr (|Split|) (emit : JSEmitInlineAttribute) exprs =
    let isOptionalParam i =
        emit.Emit.Contains (sprintf "{?%i}" i)
    let isArrayParam i =
        emit.Emit.Contains (sprintf "{%i...}" i) &&
        not(emit.Emit.Contains (sprintf "{%i" (i+1))) &&
        not(emit.Emit.Contains (sprintf "{?%i" (i+1)))
    let decls, refs =
        exprs |> List.mapi (fun i ->
            function
            | OptionPattern None when isOptionalParam i -> []
            | OptionPattern(Some(Split(decls, ref))) when isOptionalParam i -> [decls, (i, ref)]
            | Patterns.NewArray(_, exprs) when isArrayParam i ->
                // Note: Assuming that this is the last parameter
                exprs |> List.mapi (fun j (Split(decls, ref)) ->
                    decls, ((i+j), ref))
            | Split(decls, ref) -> [decls, (i, ref)])
        |> List.concat
        |> List.toArray
        |> Array.unzip
    let refMap = refs |> Map.ofArray

    decls |> Seq.concat |> Seq.toList,
    EmitExpr(fun (padding, scope) ->
        let rec build i acc =
            let next =
                match refMap.TryFind i with
                | None -> 
                    acc |> tryReplace (sprintf ", {?%i}" i) ""
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf ", {%i...}" i) "")
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf ",{%i...}" i) "")
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf "{?%i}" i) "")
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf "{%i...}" i) "")
                | Some (ref : JSExpr) ->
                    acc |> tryReplace (sprintf "{%i}" i) (ref.Print(padding, scope))
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf "{?%i}" i) (ref.Print(padding, scope)))
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf ", {%i...}" i) (sprintf ", %s, {%i...}" (ref.Print(padding, scope)) (i+1)))
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf ",{%i...}" i) (sprintf ", %s, {%i...}" (ref.Print(padding, scope)) (i+1)))
                    |> orElse (fun () ->
                        acc |> tryReplace (sprintf "{%i...}" i) (sprintf "%s, {%i...}" (ref.Print(padding, scope)) (i+1)))
            match next with
            | None -> acc
            | Some nextAcc -> build (i+1) nextAcc
        build 0 emit.Emit)


let private (|JSEmitInlineMethod|_|) (mi : MethodInfo) =
    match mi.GetCustomAttribute<JSEmitInlineAttribute>() with
    | x when x = Unchecked.defaultof<_> -> None
    | attr -> Some(mi, attr)
        
let (@.) x ys =
    match x with
    | Some y -> y :: ys
    | None -> ys

let private jsEmitInlineMethodCalling =
    CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
        function
        | Patterns.Call(objExpr, JSEmitInlineMethod(mi, attr), exprs) ->
            let allExprs = objExpr @. exprs |> List.toArray
            let argExprs, k =
                let nameParts = mi.Name.Split [|'.'|]
                if nameParts |> Array.exists (fun part -> part.StartsWith "set_") then
                    let (Split(valDecl, valExpr)) = allExprs.[allExprs.Length - 1]
                    allExprs.[.. allExprs.Length - 1] |> Array.toList,
                    fun (propDecls, propExpr) ->
                        valDecl @ propDecls @ [Assign(propExpr, valExpr)],
                        JSExpr.Null
                else allExprs |> Array.toList, id
            let decls, ref =
                createEmitInlineExpr (|Split|) attr argExprs
                |> k
            [
                yield! decls
                yield returnStategy.Return ref
            ]
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
      let methRef = createGlobalMethod name compiler mi Quote.MethodCall
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
         let isField = 
            let mapping = pi.GetCustomAttribute<CompilationMappingAttribute>()
            mapping <> Unchecked.defaultof<_> &&
            mapping.SourceConstructFlags = SourceConstructFlags.Value
         let isModuleLetBound =
            let dt = pi.DeclaringType
            let mapping = dt.GetCustomAttribute<CompilationMappingAttribute>()
            mapping <> Unchecked.defaultof<_> &&
            mapping.SourceConstructFlags = SourceConstructFlags.Module
         match isField || isModuleLetBound, objExpr, exprs with
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
   jsEmitInlineMethodCalling
   methodCalling
   propertyGetting
   propertySetting
   constructingInstances
   fieldGetting
   fieldSetting
]