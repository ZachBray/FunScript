module internal FunJS.ExpressionReplacer

open InternalCompiler
open CompilerComponent
open Microsoft.FSharp.Quotations
open System.Reflection
open System

let private buildInlineReplacement (mi:MethodBase) (args:_ list) =
   match Expr.TryGetReflectedDefinition mi with
   | Some (DerivedPatterns.Lambdas(vars, bodyExpr)) -> 
      let appliedVars = 
         vars |> List.concat |> Seq.take args.Length
         |> Seq.toList
      let unappliedVars =
         vars |> List.concat |> Seq.skip args.Length
         |> Seq.toList
      let varValues = List.zip appliedVars args |> Map.ofList
      let inlinedBody = bodyExpr.Substitute(varValues.TryFind)
      let rec buildLambda expr args =
         match args with
         | [] -> expr
         | x::xs -> buildLambda (Expr.Lambda(x, expr)) xs
      buildLambda inlinedBody unappliedVars
   | _ -> failwith "Expected a method"
   
let inline (!=) x y = not <| obj.ReferenceEquals(x, y)

let private getReplacementMethod (replacementMi:MethodInfo) args =
   if replacementMi.IsGenericMethodDefinition then
      replacementMi.MakeGenericMethod(args)
   else if replacementMi.IsGenericMethod then
      replacementMi.GetGenericMethodDefinition().MakeGenericMethod(args)
   else replacementMi

let private isInlined (replacementMi:MethodInfo) = 
   replacementMi.GetCustomAttribute<InlineAttribute>() != null ||
   replacementMi.DeclaringType.GetCustomAttribute<InlineAttribute>() != null

let createMethodMap mi replacementMi =
   let isInlined = isInlined replacementMi
   let replacementRegistration =
      if isInlined then None
      else Some replacementMi
   createCallerReplacer mi replacementRegistration <| fun split compiler retStrategy ->
      function
      | None, args, exprs -> 
         let mi = getReplacementMethod replacementMi args
         compiler.Compile retStrategy <|
            if isInlined then buildInlineReplacement mi exprs
            else Expr.Call(mi, exprs)
      | Some obj, args, exprs -> 
         let mi = getReplacementMethod replacementMi args
         compiler.Compile retStrategy <|
            if isInlined then buildInlineReplacement mi (obj::exprs)
            else Expr.Call(mi, obj::exprs)

let createTypeMethodMappings (fromType:Type) (toType:Type) =
   let methodLookup = 
      toType.GetMethods() 
      |> Array.map (fun mi -> mi.Name, mi)
      |> Map.ofArray
   let availableMethods =
      fromType.GetMethods(BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
      |> Array.choose (fun mi ->
         match methodLookup.TryFind mi.Name with
         | Some replacementMi -> 
            match Expr.TryGetReflectedDefinition replacementMi with
            | Some _ -> 
               let replacementMi =
                  if mi.IsGenericMethod then
                     replacementMi.MakeGenericMethod(mi.GetGenericArguments())
                  else replacementMi
               Some <| createMethodMap mi replacementMi
            | None -> None
         | None -> None)
      |> Array.toList
   availableMethods

let private getModule assembly name =
   let ass =
      AppDomain.CurrentDomain.GetAssemblies()
      |> Seq.find (fun ass -> ass.GetName().Name = assembly)
   ass.GetType name

let createModuleMapping fromAss fromType toAss toType =
   createTypeMethodMappings (getModule fromAss fromType) (getModule toAss toType)

let create (quoteTemplate:Expr<'a>) (quoteReplacement:Expr<'a>) =
   let replacementMi = Quote.toMethodInfo quoteReplacement
   let mi = Quote.toMethodInfo quoteTemplate
   createMethodMap mi replacementMi


let createGetter (quoteTemplate:Expr<'a>) (quoteReplacement:Expr<'a>) =
   let replacementMi = Quote.toMethodInfo quoteReplacement
   let mi = Quote.toPropertyInfo quoteTemplate
   let isInlined = isInlined replacementMi
   let replacementRegistration =
      if isInlined then None
      else Some replacementMi
   createGetterReplacer mi replacementRegistration <| fun split compiler retStrategy ->
      function
      | None, args, exprs -> 
         let mi = getReplacementMethod replacementMi args
         compiler.Compile retStrategy <|
            if isInlined then buildInlineReplacement replacementMi exprs
            else Expr.Call(replacementMi, exprs)
      | Some obj, args, exprs -> 
         let mi = getReplacementMethod replacementMi args
         compiler.Compile retStrategy <|
            if isInlined then buildInlineReplacement mi (obj::exprs)
            else Expr.Call(mi, obj::exprs)

