module internal FunScript.Comparison

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System

[<JS; Inline>]
module Replacements =
   
   let min x y = if y < x then y else x

   let max x y = if y > x then y else x

[<JS>]
module EmittedReplacements =
   [<JSEmitInline("({0} == {1})")>]
   let referenceEquals(x : obj, y : obj) : bool = failwith "never"

   [<JSEmitInline("({0} < {1} ? -1 : ({0} == {1} ? 0 : 1))")>]
   let compare(x: obj, y:obj) : bool = failwith "never"

   [<JSEmitInline("({0} == {1})")>]
   let eq(x: obj, y:obj) : bool = failwith "never"
   
   [<JSEmitInline("({0} != {1})")>]
   let neq(x: obj, y:obj) : bool = failwith "never"

   [<JSEmitInline("({0} < {1})")>]
   let lt(x: obj, y:obj) : bool = failwith "never"

   [<JSEmitInline("({0} <= {1})")>]
   let lte(x: obj, y:obj) : bool = failwith "never"

   [<JSEmitInline("({0} > {1})")>]
   let gt(x: obj, y:obj) : bool = failwith "never"

   [<JSEmitInline("({0} >= {1})")>]
   let gte(x: obj, y:obj) : bool = failwith "never"

type private TypeKind =
   | Primitive
   | NonPrimitive

/// obj and unit will be considered primitive for comparison in Javascript
let rec private kindOf (t:System.Type) =
   if t = typeof<obj> || t = typeof<unit> || Reflection.isPrimitive t then Primitive
   elif t.IsArray then 
      if t.GetArrayRank() <> 1 then
         failwith "Unsupported array rank."
      NonPrimitive
   else NonPrimitive

let compareToMethod = typeof<System.IComparable>.GetMethod("CompareTo")
let compareMethod, _ = Quote.toMethodInfoFromLambdas <@ compare @>

let hasInterface (t:Type) (interfaceType:Type) =
    t.GetInterfaces() 
    |> Array.exists (fun t -> t.GUID = interfaceType.GUID)

let tryGetCompareToMethod compiler t =
    if not (hasInterface t typeof<IComparable>) then None
    else
        let mapping = t.GetInterfaceMap typeof<IComparable>
        let compareToImplMethod = mapping.TargetMethods |> Seq.head
        ReflectedDefinitions.tryCreateGlobalMethod "CompareTo" compiler compareToImplMethod Quote.CallType.MethodCall
    
let compareInOrder valueExtractors comparandA comparandB =
    Array.foldBack (fun extractValue otherwise ->
        let aValue : Expr = extractValue comparandA
        let bValue = extractValue comparandB
        let typedCompareMethod = compareMethod.MakeGenericMethod [|aValue.Type|]
        let diffExpr = Expr.Call(typedCompareMethod, [aValue; bValue])
        <@
            let diff = (%%diffExpr : int)
            if diff <> 0 then diff
            else %otherwise
        @>) valueExtractors <@ 0 @>

// TODO: Binding flags?

let compareUnion t comparandA comparandB =
    let ucis = FSharpType.GetUnionCases(t, true)
    Array.foldBack (fun uci otherwise ->
        let isAThisCase = Expr.UnionCaseTest(comparandA, uci)
        let isBThisCase = Expr.UnionCaseTest(comparandB, uci)
        let valueExtractors =
            uci.GetFields() 
            |> Array.map (fun pi -> fun expr -> Expr.PropertyGet(expr, pi))
        let thisCaseComparison =
            compareInOrder valueExtractors comparandA comparandB
        let caseValue = uci.Tag
        <@  let isAThisCase = %%isAThisCase
            let isBThisCase = %%isBThisCase
            if isAThisCase && isBThisCase then 
                %thisCaseComparison
            elif isAThisCase then -1
            elif isBThisCase then 1
            else %otherwise @>
    ) ucis <@ failwith "Unknown union case" @>
    
type Helper() =
    [<JS>]
    static member ArrayCompare<'a when 'a : comparison>(xs:'a[], ys:'a[]) =
        if xs.Length < ys.Length then -1
        elif xs.Length > ys.Length then 1
        else Seq.compareWith compare xs ys

    [<JSEmitInline("({0}({1}, {2}))")>]
    static member Apply(f:'a -> 'a -> 'b, x:'a, y:'a) : 'b = failwith "never"

let arrayCompareMethod =
    typeof<Helper>.GetMethod(
        "ArrayCompare", 
        Reflection.BindingFlags.Public 
        ||| Reflection.BindingFlags.NonPublic
        ||| Reflection.BindingFlags.Static)

let applyMethod =
    typeof<Helper>.GetMethod(
        "Apply", 
        Reflection.BindingFlags.Public 
        ||| Reflection.BindingFlags.NonPublic
        ||| Reflection.BindingFlags.Static)

let compareArray elementType comparandA comparandB =
    let typeCompareMethod = arrayCompareMethod.MakeGenericMethod [|elementType|]
    Expr.Call(typeCompareMethod, [comparandA; comparandB])

let createCompareLambda t createComparisonBody =
    let a = Var("a", t)
    let b = Var("b", t)
    Expr.Lambda(a, Expr.Lambda(b, createComparisonBody (Expr.Var a) (Expr.Var b)))

let tryAutoGenerateCompareToLambda t =
    if FSharpType.IsTuple t then
        Some(fun () ->
            let valueExtractors =
                FSharpType.GetTupleElements t 
                |> Array.mapi (fun i _ -> fun expr -> Expr.TupleGet(expr, i))
            compareInOrder valueExtractors |> createCompareLambda t)
    elif FSharpType.IsRecord t then
        Some(fun () ->
            let valueExtractors =
                FSharpType.GetRecordFields(t, true) 
                |> Array.map (fun pi -> fun expr -> Expr.PropertyGet(expr, pi))
            compareInOrder valueExtractors |> createCompareLambda t)
    elif FSharpType.IsUnion t then
        Some(fun () -> compareUnion t |> createCompareLambda t)
    elif t.IsArray then
        Some(fun () -> 
            let elementType = t.GetElementType()
            compareArray elementType |> createCompareLambda t)
    else None

let getCompareToName compiler t =
   let typeArgs = Reflection.getGenericTypeArgs t
   let specialization = Reflection.getSpecializationString compiler typeArgs
   JavaScriptNameMapper.mapType t + "_" + "GeneratedCompareTo" + specialization

let comparableCompareComponent compareQuote =
    let primitiveCompareMethod, _ = Quote.toMethodInfoFromLambdas <@ EmittedReplacements.compare @>
    CompilerComponent.generateBinaryMacro compareQuote (fun _ compiler _ exprA exprB ->
        if exprA.Type <> exprB.Type then None
        else
            match kindOf exprA.Type, kindOf exprB.Type with
            | Primitive, Primitive -> 
                Some(Expr.Call(primitiveCompareMethod, [exprA; exprB]))
            | NonPrimitive, NonPrimitive ->
                match tryGetCompareToMethod compiler exprA.Type with
                | Some compareFuncVar ->
                    let lambda = Expr.Var compareFuncVar
                    let innerLambdaType = typedefof<_ -> _>.MakeGenericType [|exprB.Type; typeof<int>|]
                    let outerLambdaType = typedefof<_ -> _>.MakeGenericType [|exprA.Type; innerLambdaType|]
                    let typedLambda = Expr.Coerce(lambda, outerLambdaType)
                    let typedApplyMethod = applyMethod.MakeGenericMethod [|exprA.Type; typeof<int>|]
                    Some(Expr.Call(typedApplyMethod, [typedLambda; exprA; exprB])) 
                    // TODO: There is something odd going on with application because we
                    //       use tryCreateGlobalMethod so we cannot use this here (yet):
                    //Some(Expr.Application(Expr.Application(typedLambda, exprA), exprB))
                | None ->
                    match tryAutoGenerateCompareToLambda exprA.Type with
                    | None -> None
                    | Some createCompareToLambda ->
                        let compareFuncVar = 
                            compiler.DefineGlobal (getCompareToName compiler exprA.Type) (fun var ->
                                compiler.Compile (ReturnStrategies.assignVar var) (createCompareToLambda())
                            )
                        let lambda = Expr.Var compareFuncVar
                        let innerLambdaType = typedefof<_ -> _>.MakeGenericType [|exprB.Type; typeof<int>|]
                        let outerLambdaType = typedefof<_ -> _>.MakeGenericType [|exprA.Type; innerLambdaType|]
                        let typedLambda = Expr.Coerce(lambda, outerLambdaType)
                        let typedApplyMethod = applyMethod.MakeGenericMethod [|exprA.Type; typeof<int>|]
                        Some(Expr.Application(Expr.Application(typedLambda, exprA), exprB))
            | _ -> None
    )

let comparisonOperator operatorQuote replacementPrimitiveQuote =
    let primitiveOperatorMethod, _ = Quote.toMethodInfoFromLambdas replacementPrimitiveQuote
    let operatorMethod, _ = Quote.toMethodInfoFromLambdas operatorQuote
    CompilerComponent.generateBinaryMacro operatorQuote (fun _ compiler _ exprA exprB ->
        if exprA.Type <> exprB.Type then None
        else
            match kindOf exprA.Type, kindOf exprB.Type with
            | Primitive, Primitive -> 
                Some(Expr.Call(primitiveOperatorMethod, [exprA; exprB]))
            | NonPrimitive, NonPrimitive -> 
                let typedOperatorMethod = operatorMethod.MakeGenericMethod [|typeof<int>|]
                let typedCompareMethod = compareMethod.MakeGenericMethod [|exprA.Type|]
                let diffExpr = Expr.Call(typedCompareMethod, [exprA; exprB])
                let zeroExpr = Expr.Value 0
                Some(Expr.Call(typedOperatorMethod, [diffExpr; zeroExpr]))
            | _ -> None
    )

let components = [
   comparisonOperator <@ (=) @> <@ EmittedReplacements.eq @>
   comparisonOperator <@ (<>) @> <@ EmittedReplacements.neq @>
   comparisonOperator <@ (>) @> <@ EmittedReplacements.gt @>
   comparisonOperator <@ (>=) @> <@ EmittedReplacements.gte @>
   comparisonOperator <@ (<) @> <@ EmittedReplacements.lt @>
   comparisonOperator <@ (<=) @> <@ EmittedReplacements.lte @>
   comparableCompareComponent <@ compare @>
   comparableCompareComponent <@ Unchecked.compare @>
   ExpressionReplacer.create <@ obj.ReferenceEquals @> <@ EmittedReplacements.referenceEquals @>
   ExpressionReplacer.create <@ min @> <@ Replacements.min @>
   ExpressionReplacer.create <@ max @> <@ Replacements.max @>
]