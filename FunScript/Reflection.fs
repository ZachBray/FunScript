module internal FunScript.Reflection

open FunScript.AST
open Microsoft.FSharp.Quotations
open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Reflection

let primitiveTypes =
   set [
      typeof<sbyte>.FullName
      typeof<byte>.FullName
      typeof<int16>.FullName
      typeof<uint16>.FullName
      typeof<int32>.FullName
      typeof<uint32>.FullName
      typeof<int64>.FullName
      typeof<uint64>.FullName
      typeof<float>.FullName
      typeof<single>.FullName
      typeof<float32>.FullName

      typeof<string>.FullName
      typeof<char>.FullName
      typeof<bool>.FullName
   ]

let isGenericParameter (t : System.Type) =
   t.IsGenericParameter

/// For generic specialization. 
let isPrimitive (t : Type) =
   primitiveTypes.Contains t.FullName

let getGenericTypeArgs (t : Type) =
   let isGeneric = t.IsGenericType || t.IsGenericTypeDefinition
   if isGeneric then t.GetGenericArguments()
   else [||]

let getGenericMethodArgs (mb : MethodBase) =
   let isGeneric = mb.IsGenericMethod || mb.IsGenericMethodDefinition
   let methodTypeArgs =
      if isGeneric then mb.GetGenericArguments()
      else [||]
   Array.append (getGenericTypeArgs mb.DeclaringType) methodTypeArgs

let getSpecializationString (compiler : InternalCompiler.ICompiler) ts =
   ts |> Seq.map (fun (t : Type) ->
      //TODO: Name isn't safe. We really need to keep a dictionary around
      // to make this safer. Using only the short names if there are no collisions.
      if isPrimitive t then t.Name
      elif compiler.ShouldFlattenGenericsForReflection then JavaScriptNameMapper.mapType t
      else "obj"
   ) 
   |> String.concat "_"
   |> JavaScriptNameMapper.sanitizeAux

let getRecordConstructorName compiler (recType : System.Type) =
   let ci = 
      recType.GetConstructors(
         BindingFlags.Public ||| 
         BindingFlags.NonPublic ||| 
         BindingFlags.Instance).[0]
   let typeArgs = getGenericTypeArgs recType
   let specialization = getSpecializationString compiler typeArgs
   JavaScriptNameMapper.mapMethod ci + specialization

let getUnionCaseConstructorName compiler (uci : UnionCaseInfo) =
   let typeArgs = getGenericTypeArgs uci.DeclaringType
   let specialization = getSpecializationString compiler typeArgs
   JavaScriptNameMapper.mapType uci.DeclaringType + "_" + uci.Name + specialization
   
let createLambdaVars(fields : Type[]) =
   let fieldCount = fields.Length
   let argsVar = Var("args", typeof<obj[]>)
   let argsExpr = Expr.Var argsVar
   let vars = 
      Array.init fieldCount (fun i -> Expr.Coerce(<@@ (%%argsExpr : obj[]).[i] @@>, fields.[i]))
      |> Array.toList
   argsVar, vars

// TODO: memoize for recursive types!
let rec private getPropertyInfoExpr (getTypeVar : Type -> Var) (pi : PropertyInfo) =
   let name = pi.Name
   let objArg = Var("obj", typeof<obj>)
   let caller = Expr.Lambda(objArg, Expr.Coerce(Expr.PropertyGet(Expr.Coerce(Expr.Var objArg, pi.DeclaringType), pi), typeof<obj>))
   let typeVar = Expr.Coerce(Expr.Var(getTypeVar pi.PropertyType), typeof<Core.Type.Type>)
   <@@ Core.Type.PropertyInfo(name, %%caller, fun () -> %%typeVar) @@>

and private getUnionCaseInfo (compiler : InternalCompiler.ICompiler) getTypeVar (uci : UnionCaseInfo) =
   let name = uci.Name
   let tag = uci.Tag
   let fields = uci.GetFields()
   let exprs = fields |> Array.map (getPropertyInfoExpr getTypeVar) |> Array.toList
   let arrayExpr = Expr.NewArray(typeof<Core.Type.PropertyInfo>, exprs)
   let argsVar, vars = createLambdaVars (fields |> Array.map (fun pi -> pi.PropertyType))
   let constructorLambda =
      Expr.Lambda(argsVar, Expr.Coerce(Expr.NewUnionCase(uci, vars), typeof<obj>))
   <@@ Core.Type.UnionCaseInfo(name, tag, %%constructorLambda, %%arrayExpr) @@>

and private getKind compiler getTypeVar t =
    if FSharpType.IsRecord t then 
        let pis = FSharpType.GetRecordFields t
        let exprs = pis |> Array.map (getPropertyInfoExpr getTypeVar) |> Array.toList
        let arrayExpr = Expr.NewArray(typeof<Core.Type.PropertyInfo>, exprs)
        let argsVar, vars = createLambdaVars (pis |> Array.map (fun pi -> pi.PropertyType))
        let constructorLambda =
            Expr.Lambda(argsVar, Expr.Coerce(Expr.NewRecord(t, vars), typeof<obj>))
        <@ Core.Type.RecordType(%%constructorLambda, %%arrayExpr) @>
    elif FSharpType.IsUnion t then
        let ucis = FSharpType.GetUnionCases t |> Array.sortBy (fun uci -> uci.Tag)
        let exprs = ucis |> Array.map (getUnionCaseInfo compiler getTypeVar) |> Array.toList
        let arrayExpr = Expr.NewArray(typeof<Core.Type.UnionCaseInfo>, exprs)
        <@ Core.Type.UnionType %%arrayExpr @>
    elif FSharpType.IsTuple t then
        let ts = FSharpType.GetTupleElements t
        let tVars = ts |> Array.map getTypeVar
        let tExprs = 
            tVars |> Array.map Expr.Var 
            |> Array.map (fun v -> Expr.Coerce(v, typeof<Core.Type.Type>)) 
            |> Array.toList
        let tsExpr = Expr.NewArray(typeof<Core.Type.Type>, tExprs)
        let argsVar, vars = createLambdaVars ts
        let constructorLambda =
            Expr.Lambda(argsVar, Expr.Coerce(Expr.NewTuple vars, typeof<obj>))
        <@ Core.Type.TupleType(%%constructorLambda, %%tsExpr) @>
    elif t.IsArray then
        let elT = t.GetElementType()
        let elTVar = getTypeVar elT
        let elTExpr = Expr.Coerce(Expr.Var elTVar, typeof<Core.Type.Type>)
        <@ Core.Type.ArrayType(%%elTExpr) @>
    else <@ Core.Type.ClassType @>

and private netTypeExpr compiler getTypeVar (t : Type) =
   let name = t.Name
   let fullName = t.FullName
   let typeArgs =
      let isGeneric = t.IsGenericType || t.IsGenericTypeDefinition
      if isGeneric then t.GetGenericArguments()
      else [||]
   let typeArgExprs = typeArgs |> Array.toList |> List.map (netTypeExpr compiler getTypeVar)
   let typeArgsArrayExpr = Expr.NewArray(typeof<Core.Type.Type>, typeArgExprs)
   let kindExpr = getKind compiler getTypeVar t
   <@@ Core.Type.Type(name, fullName, %%typeArgsArrayExpr, %kindExpr) @@>

let rec buildRuntimeType (compiler : InternalCompiler.ICompiler) (t : System.Type) =
   let typeName = sprintf "t_%s" (JavaScriptNameMapper.mapType t)
   compiler.DefineGlobal typeName (fun var ->
      let expr = netTypeExpr compiler (buildRuntimeType compiler) t
      compiler.Compile (ReturnStrategies.assignVar var) expr
   )

let components = 
   [
      [
         CompilerComponent.generateArityWithCompiler 
            <@ fun x -> x.GetType() @> 
            (fun _ _ (|Split|) compiler -> 
               function
               | [Split(decls, ref) as expr] ->
                  let typeVar =
                     if isGenericParameter expr.Type then
                        failwith "Expected all types to be flattened"
                     else buildRuntimeType compiler expr.Type
                  Some([decls], Reference typeVar)
               | _ -> None)

         CompilerComponent.generateArityWithCompiler
            <@ typeof<_> @>
            (fun _ typeArgs (|Split|) compiler ->
               function
               | [] ->
                  match typeArgs with
                  | [|t|] -> 
                     let typeVar =
                        if isGenericParameter t then 
                           failwith "Expected all types to be flattened"
                        else buildRuntimeType compiler t
                     Some([], Reference typeVar)
                  | _ -> None
               | _ -> None)

      ]

      ExpressionReplacer.createTypeMethodMappings typeof<System.Reflection.PropertyInfo> typeof<Core.Type.PropertyInfo>
      ExpressionReplacer.createTypeMethodMappings typeof<Microsoft.FSharp.Reflection.UnionCaseInfo> typeof<Core.Type.UnionCaseInfo>
      ExpressionReplacer.createTypeMethodMappings typeof<Microsoft.FSharp.Reflection.FSharpType> typeof<Core.Type.FSharpType>
      ExpressionReplacer.createTypeMethodMappings typeof<Microsoft.FSharp.Reflection.FSharpValue> typeof<Core.Type.FSharpValue>
      ExpressionReplacer.createTypeMethodMappings typeof<System.Type> typeof<Core.Type.Type>

   ] |> List.concat