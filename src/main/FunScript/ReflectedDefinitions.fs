module internal FunScript.ReflectedDefinitions

open AST
open Quote
open Microsoft.FSharp.Quotations
open System
open System.Reflection
open Microsoft.FSharp.Reflection

type MissingReflectedDefinitionException(mb: MethodBase) =
    inherit System.Exception(
        "No replacement for " +
        (if mb.DeclaringType.FullName = null then mb.Name else mb.DeclaringType.FullName + "." + mb.Name) + ": " +
        "either ReflectedDefinition attribute is missing or " +
        "the method is not yet implemented in FunScript.")

let private (|List|) = Option.toList

let private (|NonNull|_|) x = 
   if obj.ReferenceEquals(x, null) then None
   else Some x

let private (|ReflectedDefinition|_|) (mi:MethodBase) =
   if mi.DeclaringType.IsInterface then Some mi.Name // TODO: Fix after changing the interface implementation
   else
      match Expr.tryGetReflectedDefinition mi with
      | Some _ -> Some(JavaScriptNameMapper.mapMethod mi)
      | _ -> None

let private replaceThisInExpr (expr : Expr) =
    let var = ref None
    let fixedExpr = expr.Substitute(fun v ->
        if v.Name = "this" then
            let thisVar = 
                match !var with
                | None -> 
                    let thisVar = Var("__this", v.Type) 
                    var := Some (v, thisVar)
                    thisVar
                | Some(_, v) -> v
            Some(Expr.Var thisVar)
        else None)
    match !var with
    | None -> expr
    | Some (originalThis, newThis) -> Expr.Let(newThis, Expr.Var originalThis, fixedExpr)

let private getJavaScriptVarsInEmitBlock(emitBlock : string) =
    emitBlock.Split [|';';'{';'}';'(';')'|] 
    |> Array.map (fun stmt -> stmt.Trim())
    |> Array.collect (fun stmt ->
        let parts = stmt.Split([|' '; ',';'\n';'\r';'\t'|], StringSplitOptions.RemoveEmptyEntries)
        if parts.Length > 1 && parts.[0] = "var" then parts.[1..]
        else [||])
    |> set

let private genMethod (mb:MethodBase) (replacementMi:MethodBase) (vars:Var list) bodyExpr var (compiler:InternalCompiler.ICompiler) =
   match replacementMi.GetCustomAttribute<JSEmitAttribute>() with
   | meth when meth <> Unchecked.defaultof<_> ->
      let code padding (scope : VariableScope ref) =
         let _, assignedNames = !scope |> addVarsToScope vars
         let jsBodyVars = 
            if assignedNames <> [] then getJavaScriptVarsInEmitBlock meth.Emit
            else Set.empty
         let conflicts = Set.intersect (set assignedNames) jsBodyVars
         let conflictResolution = 
             conflicts |> Seq.map (fun name -> sprintf "var $_%s = %s;" name name)
             |> String.concat ""
         let body =
             vars
             |> List.zip assignedNames
             |> List.mapi (fun i v -> i,v)
             |> List.fold (fun (acc:string) (i,(name, v)) ->
                let replacement =
                    if conflicts.Contains name then sprintf "$_%s" name
                    else (Reference v).Print(padding, scope)
                acc.Replace(sprintf "{%i}" i, replacement)
                ) meth.Emit
         conflictResolution + body
      [ Assign(Reference var, Lambda(vars, Block[EmitStatement(fun (padding, scope) -> code padding scope)])) ]
   | _ when mb.IsConstructor ->
      
      let fixedBodyExpr = replaceThisInExpr bodyExpr
      [
         Assign(Reference var, Lambda(vars, Block(compiler.Compile ReturnStrategies.returnFrom fixedBodyExpr)))
      ]
   | _ -> 
      [ Assign(
         Reference var, 
         Lambda(vars, 
            Block(compiler.Compile ReturnStrategies.returnFrom bodyExpr))) ]

let private deconstructTuple (tupleVar : Var) =
    if tupleVar.Type = typeof<unit> then
        [tupleVar], Expr.Value(())
    else
        let elementTypes = FSharpType.GetTupleElements tupleVar.Type
        let elementVars =
            elementTypes |> Array.mapi (fun i elementType ->
                Var(sprintf "%s_%i" tupleVar.Name i, elementType, tupleVar.IsMutable))
            |> Array.toList
        let elementExprs = elementVars |> List.map Expr.Var
        let tupleConstructionExpr =
            match elementExprs with
            | [] -> Expr.Value(()) 
            | _ -> Expr.NewTuple elementExprs
        elementVars, tupleConstructionExpr

let private extractVars (mb : MethodBase) (argCounts : CompilationArgumentCountsAttribute) = function
    | DerivedPatterns.Lambdas(vars, bodyExpr) ->
        let instanceVar, argVars =
            if mb.IsStatic || mb.IsConstructor then None, vars
            elif vars.Head.Length <> 1 then failwith "Unexpected argument format"
            else Some vars.Head.[0], vars.Tail
        let actualArgCounts =
            let hasCounts = argCounts <> Unchecked.defaultof<_>
            if hasCounts then argCounts.Counts |> Seq.toList |> Some
            else None
        let expectedArgCount = 
            let baseParamCount = max 1 (mb.GetParameters().Length)
            match actualArgCounts with
            | None -> baseParamCount
            | Some counts -> max baseParamCount (counts |> Seq.sum)
        let groupCounts =
            match actualArgCounts with
            | None -> argVars |> List.map List.length
            | Some counts -> counts
        let bodyExpr, freeArgVars = 
            List.zip groupCounts argVars
            |> List.fold (fun (totalCount, groups) (groupCount, varGroup) ->
                let subTotal = groupCount + totalCount
                subTotal, (subTotal, groupCount, varGroup) :: groups) (0, [])
            |> snd
            |> List.fold (fun (restExpr, freeVars) (subTotal, groupCount, varGroup) ->
                if subTotal > expectedArgCount && 
                   subTotal - groupCount >= expectedArgCount then
                    if varGroup.Length = 1 then 
                        Expr.Lambda(varGroup.[0], restExpr), freeVars
                    elif varGroup.Length = groupCount then
                        failwith "todo"
                    else failwith "Unexpected argument format"
                elif subTotal > expectedArgCount then
                    failwith "Unexpected argument format"
                else
                    if varGroup.Length = groupCount then
                        restExpr, varGroup @ freeVars
                    elif varGroup.Length = 1 then
                        let tupleVar = varGroup.[0]
                        let elementVars, tupleConstructionExpr =
                            deconstructTuple tupleVar
                        Expr.Let(tupleVar, tupleConstructionExpr, restExpr), elementVars @ freeVars
                    else
                        failwith "Unexpected argument format") (bodyExpr, [])
        let freeVars =
            match instanceVar with
            | None -> freeArgVars
            | Some ivar -> ivar :: freeArgVars
        freeVars, bodyExpr
    | expr -> [], expr

let methodCallPattern (mb:MethodBase) =
    let argCounts = mb.GetCustomAttribute<CompilationArgumentCountsAttribute>()
    match Expr.tryGetReflectedDefinition mb with
    | Some fullExpr -> Some(fun () -> extractVars mb argCounts fullExpr)
    | None -> None

let (|CallPattern|_|) = methodCallPattern

let replaceIfAvailable (compiler:InternalCompiler.ICompiler) (mb : MethodBase) callType =
   match compiler.ReplacementFor mb callType with
   | None -> mb //GetGenericMethod()...
   | Some mi -> upcast mi

let tryCreateGlobalMethod name compiler mb callType =
   match replaceIfAvailable compiler mb callType with
   | CallPattern getVarsExpr as replacementMi ->
      let typeArgs = Reflection.getGenericMethodArgs replacementMi
      let specialization = Reflection.getSpecializationString compiler typeArgs
      Some(
         compiler.DefineGlobal (name + specialization) (fun var ->
            let vars, bodyExpr = getVarsExpr()
            genMethod mb replacementMi vars bodyExpr var compiler))
   | _ -> None

let createGlobalMethod name compiler mb callType =
    match tryCreateGlobalMethod name compiler mb callType with
    | None -> raise <| MissingReflectedDefinitionException mb
    | Some x -> x

let getObjectConstructorVar compiler ci =
   match ci with
   | ReflectedDefinition name ->
      createGlobalMethod name compiler ci Quote.ConstructorCall
   | _ -> raise <| MissingReflectedDefinitionException ci

let private createConstruction
      (|Split|) 
      (returnStategy:InternalCompiler.IReturnStrategy)
      (compiler:InternalCompiler.ICompiler)
      (exprs: seq<Expr list>)
      (ci: MethodBase) =
    let decls, refs =
        exprs |> List.concat |> Reflection.getDeclarationAndReferences (|Split|)
    let call =
        if FSharpType.IsExceptionRepresentation ci.DeclaringType then
            let cons = Reflection.getCustomExceptionConstructorVar compiler ci
            New(cons, refs)
        else
            let cons = getObjectConstructorVar compiler ci
            if Reflection.isPrimaryConstructor ci
            then New(cons, refs)
            else Apply(Reference cons, refs) // Secondary constructors don't need new keyword
    [ yield! decls |> List.concat
      yield returnStategy.Return call ]

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
   let decls, refs = Reflection.getDeclarationAndReferences (|Split|) exprs
   match mi with
   | SpecialOp((ReflectedDefinition name) as mi)
   | (ReflectedDefinition name as mi) when mi.DeclaringType.IsInterface ->
      match refs with
      | [] -> []
      | objRef::argRefs ->
         [  yield! decls |> List.concat
            yield returnStategy.Return <| Apply(PropertyGet(objRef, name), argRefs) ] // TODO: Fix after changing the interface implementation
   | SpecialOp((ReflectedDefinition name) as mi)
   | (ReflectedDefinition name as mi) ->
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
   CompilerComponent.create <| fun split compiler returnStrategy ->
      function
      // F# Custom exceptions // TODO: refactor?
      | Patterns.PropertyGet(Some(Patterns.Coerce(Patterns.Var var, t)), pi, exprs)
        when FSharpType.IsExceptionRepresentation pi.DeclaringType && pi.Name.StartsWith "Data" ->
         [ returnStrategy.Return <| PropertyGet(Reference var, pi.Name) ]
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
            [ returnStrategy.Return <| Reference property ]
         | _ -> createCall split returnStrategy compiler [objExpr; exprs] (pi.GetGetMethod(true))
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

let private constructingInstances =
   CompilerComponent.create <| fun split compiler returnStrategy ->
      function
      | PatternsExt.NewObject(ci, exprs) -> 
         if ci.DeclaringType.GUID = typeof<obj>.GUID &&
            ci.DeclaringType.FullName = typeof<obj>.FullName // Empty objects
         then [ returnStrategy.Return <| JSExpr.Object [] ]
         else createConstruction split returnStrategy compiler [exprs] ci
      // Creating instances of generic types with parameterless constructors (e.g. new T'())
      | Patterns.Call(None, mi, []) when mi.Name = "CreateInstance" && mi.IsGenericMethod ->
         let t = mi.GetGenericArguments().[0]
         let ci = t.GetConstructor([||])
         createConstruction split returnStrategy compiler [] ci
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