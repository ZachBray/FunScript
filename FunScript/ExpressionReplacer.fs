module (*internal*) FunScript.ExpressionReplacer

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
      // This is to fix the problem where the inlined function
      // doesn't use all its arguments and they might not be evaluated
      let varValuesUsed = ref Set.empty
      let inlinedBody = bodyExpr.Substitute(fun x -> 
         match varValues.TryFind x with
         | Some expr -> 
            varValuesUsed := !varValuesUsed |> Set.add x
            Some expr
         | None -> None)
      let varValuesUsed = !varValuesUsed
      let varValuesIgnored = 
         varValues |> Map.filter (fun var _ -> not (varValuesUsed.Contains var))
      let _, wholeBody =
         varValuesIgnored |> Map.fold (fun (n,rest) v expr ->
            n+1, Expr.Let(Var(sprintf "ignored%i" n, expr.Type), expr, rest)
         ) (0, inlinedBody)
      let rec buildLambda expr args =
         match args with
         | [] -> expr
         | x::xs -> buildLambda (Expr.Lambda(x, expr)) xs
      buildLambda wholeBody unappliedVars
   | _ -> failwith "Expected a method"
   

let inline (==) x y = obj.ReferenceEquals(x, y)
let inline (!=) x y = not (x == y)

let private getReplacementMethod (replacementMi:MethodInfo) args =
   if replacementMi.IsGenericMethodDefinition then
      replacementMi.MakeGenericMethod(args)
   else if replacementMi.IsGenericMethod then
      replacementMi.GetGenericMethodDefinition().MakeGenericMethod(args)
   else replacementMi

let private isInlined (replacementMi:MethodInfo) = 
   (replacementMi.GetCustomAttribute<InlineAttribute>() != null ||
    replacementMi.DeclaringType.GetCustomAttribute<InlineAttribute>() != null) &&
   (replacementMi.GetCustomAttribute<JSEmitAttribute>() == null)

let private castMi =
   typeof<Helpers>
      .GetMethod("Cast", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
      .GetGenericMethodDefinition()

let buildCall (mi:MethodInfo) exprs =
   let obj, argExprs =
      match exprs with
      | objExpr::argExprs when not mi.IsStatic ->
         Some objExpr, argExprs
      | exprs -> None, exprs
   let castArgs = 
      mi.GetParameters() |> Array.toList
      |> List.map2 (fun expr p ->
         let castArg = castMi.MakeGenericMethod p.ParameterType
         Expr.Call(castArg, [expr])
      ) argExprs
   match obj with
   | None -> Expr.Call(mi, castArgs)
   | Some objExpr ->
      let objType = mi.DeclaringType
      let castArg = castMi.MakeGenericMethod objType
      let castObjExpr = Expr.Call(castArg, [objExpr])
      Expr.Call(castObjExpr, mi, castArgs)

let createMethodMap mi callType replacementMi =
   let isInlined = isInlined replacementMi
   let replacementRegistration =
      if isInlined then None
      else Some replacementMi
   createCallerReplacer mi callType replacementRegistration <| fun split compiler retStrategy ->
      function
      | None, args, exprs -> 
         let mi = getReplacementMethod replacementMi args
         compiler.Compile retStrategy <|
            if isInlined then buildInlineReplacement mi exprs
            else buildCall mi exprs
      | Some obj, args, exprs -> 
         let mi = getReplacementMethod replacementMi args
         compiler.Compile retStrategy <|
            if isInlined then buildInlineReplacement mi (obj::exprs)
            else buildCall mi (obj::exprs)

let createTypeMethodMappings (fromType:Type) (toType:Type) =
   let methodLookup = 
      toType.GetMethods(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
      |> Array.map (fun mi -> mi.Name, mi)
      |> Map.ofArray
   let availableMethods =
      fromType.GetMethods(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
      |> Array.choose (fun mi ->
         match methodLookup.TryFind mi.Name with
         | Some replacementMi -> 
            match Expr.TryGetReflectedDefinition replacementMi with
            | Some _ -> 
               let replacementMi =
                  if mi.IsGenericMethod then
                     if replacementMi.IsGenericMethod then
                        replacementMi.MakeGenericMethod(mi.GetGenericArguments())
                     else
                        replacementMi
                  else replacementMi
               Some <| createMethodMap mi Quote.MethodCall replacementMi
            | None -> None
         | None -> None)
      |> Array.toList
   availableMethods

let private getModule assembly (name:string) =
   let ass =
      AppDomain.CurrentDomain.GetAssemblies()
      |> Seq.find (fun ass -> ass.GetName().Name = assembly)
   let split (name:string) =
      let i = name.LastIndexOf '.'
      if i >= 0 then Some(name.Substring(0, i), name.Substring (i+1))
      else None
   let rec getType n =
      let t = ass.GetType n
      if obj.ReferenceEquals(t, null) then
         match split n with
         | Some(parentType, typeName) ->
            let parentT: System.Type = getType parentType
            parentT.GetMember(typeName).[0] :?> System.Type
         | None -> failwithf "Could not find type: %s" name
      else t
   getType name

let createModuleMapping fromAss fromType toAss toType =
   createTypeMethodMappings (getModule fromAss fromType) (getModule toAss toType)

let private createImpl (quoteTemplate:Expr) (quoteReplacement:Expr) =
   let replacementMi, _ = Quote.toMethodInfoFromLambdas quoteReplacement
   let mi, callType = Quote.toMethodBaseFromLambdas quoteTemplate
   createMethodMap mi callType replacementMi

let createUnsafe (quoteTemplate:Expr) (quoteReplacement:Expr) =
   createImpl quoteTemplate quoteReplacement

let create (quoteTemplate:Expr<'a>) (quoteReplacement:Expr<'a>) =
   createImpl quoteTemplate quoteReplacement