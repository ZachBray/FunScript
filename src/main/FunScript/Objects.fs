module (*internal*) FunScript.Objects

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System

let rec private isAvailable (compiler : InternalCompiler.ICompiler) (mb : MethodBase) =
      mb <> null && (
         let t = mb.DeclaringType
         FSharpType.IsUnion(t, BindingFlags.Public ||| BindingFlags.NonPublic)  || 
         FSharpType.IsRecord(t, BindingFlags.Public ||| BindingFlags.NonPublic) || 
         Expr.tryGetReflectedDefinition(mb).IsSome ||
         compiler.ReplacementFor mb Quote.CallType.MethodCall |> Option.exists (isAvailable compiler))

let private propertyGetter =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.PropertyGet(Some(Split(objDecl, objRef)), pi, [])
            when isAvailable compiler (pi.GetGetMethod(true))
            ->
         [ yield! objDecl 
           yield returnStategy.Return <| PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux pi.Name)
         ]
      | _ -> []

let private propertySetter =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.PropertySet(Some(Split(objDecl, objRef)), pi, [], Split(valDecl, valRef))
            when isAvailable compiler (pi.GetSetMethod(true))
            ->
         [ yield! objDecl 
           yield! valDecl
           yield Assign(PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux pi.Name), valRef)
           if returnStategy = ReturnStrategies.inplace then
               yield returnStategy.Return Null 
         ]
      | _ -> []


let localized (name:string) =
   let sections = name.Split '-'
   JavaScriptNameMapper.sanitizeAux sections.[sections.Length - 1]

let private getAllMethods (t:System.Type) =
   t.GetMethods(
      BindingFlags.Public ||| 
      BindingFlags.NonPublic ||| 
      BindingFlags.FlattenHierarchy ||| 
      BindingFlags.Instance)

let replaceIfAvailable (compiler:InternalCompiler.ICompiler) (mb : MethodBase) callType =
   match compiler.ReplacementFor mb callType with
   | None -> mb //GetGenericMethod()...
   | Some mi -> upcast mi

let deconstructTuple (tupleVar : Var) =
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

let extractVars (mb : MethodBase) (argCounts : CompilationArgumentCountsAttribute) = function
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

let getFields (t:Type) =
   t.GetProperties(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
   |> Seq.map (fun p -> p, p.GetCustomAttribute<CompilationMappingAttribute>())
   |> Seq.filter (fun (p, attr) -> not <| obj.ReferenceEquals(null, attr)) 
   |> Seq.filter (fun (p, attr) -> SourceConstructFlags.Field = attr.SourceConstructFlags)
   |> Seq.sortBy (fun (p, attr) -> attr.SequenceNumber)
   |> Seq.map (fun (p, attr) -> JavaScriptNameMapper.sanitizeAux p.Name, p.PropertyType)
   |> Seq.toList

let components = [ 
   propertyGetter
   propertySetter
]