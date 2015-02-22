module internal FunScript.Reflection

open FunScript.AST
open Microsoft.FSharp.Quotations
open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Reflection

let jsNumberTypes =
   set [
      typeof<sbyte>.FullName
      typeof<byte>.FullName
      typeof<int16>.FullName
      typeof<uint16>.FullName
      typeof<int32>.FullName
      typeof<uint32>.FullName
      typeof<int64>.FullName
      typeof<uint64>.FullName
      typeof<float32>.FullName
      typeof<float>.FullName
      typeof<TimeSpan>.FullName // Treated as number
   ]

let jsStringTypes =
   set [
      typeof<string>.FullName
      typeof<char>.FullName
      typeof<Guid>.FullName // Treated as string
   ]

let isGenericParameter (t : System.Type) =
   t.IsGenericParameter

/// For generic specialization
let isPrimitive (t : Type) =
   jsNumberTypes.Contains t.FullName || t.IsEnum ||
   jsStringTypes.Contains t.FullName ||
   t = typeof<bool>

let isPrimaryConstructor (ci: MethodBase) =
    JavaScriptNameMapper.getConstructorIndex ci = 0

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
      else JavaScriptNameMapper.mapType t
   ) 
   |> String.concat "_"
   |> JavaScriptNameMapper.sanitizeAux

let getDeclarationAndReferences (|Split|) exprs =
    exprs 
    |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
    |> List.unzip

let getFields (t:Type) =
   t.GetProperties(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
   |> Seq.map (fun p -> p, p.GetCustomAttribute<CompilationMappingAttribute>())
   |> Seq.filter (fun (p, attr) -> not <| obj.ReferenceEquals(null, attr)) 
   |> Seq.filter (fun (p, attr) -> SourceConstructFlags.Field = attr.SourceConstructFlags)
   |> Seq.sortBy (fun (p, attr) -> attr.SequenceNumber)
   |> Seq.map (fun (p, attr) -> JavaScriptNameMapper.sanitizeAux p.Name, p.PropertyType)
   |> Seq.toList

let getTupleVars preffix n =
    [ 0 .. n - 1] |> List.map (fun i -> Var(sprintf "%s%i" preffix i, typeof<obj>))

let getCustomExceptionConstructorVar (compiler: InternalCompiler.ICompiler) (ci: MethodBase) =
    let createConstructor compiler n =
        let vars = getTupleVars "d" n
        let this = Var("__this", typeof<obj>)
        vars, Block
         [ yield CopyThisToVar(this)
           yield! vars |> List.mapi (fun i var ->
            Assign(PropertyGet(Reference this, sprintf "Data%i" i), Reference var)) ]
    let name = JavaScriptNameMapper.mapMethod ci
    let argsLength = ci.GetParameters() |> Array.length
    compiler.DefineGlobal name (fun var -> 
        [Assign(Reference var, Lambda <| createConstructor compiler argsLength)])

let getTupleConstructorVar compiler (typeArgs: Type list) =
    let createConstructor compiler n =
       let vars = getTupleVars "Item" n
       let refs = vars |> List.map Reference
       let this = Var("__this", typeof<obj>)
       vars, Block
        [ yield CopyThisToVar(this)
          yield Assign(PropertyGet(Reference this, "Items"), JSExpr.Array refs) ]
    let specialization = getSpecializationString compiler typeArgs
    let name = sprintf "Tuple%s" specialization
    compiler.DefineGlobal name (fun var -> 
        [Assign(Reference var, Lambda <| createConstructor compiler typeArgs.Length)])

let getRecordVars recType =
   getFields recType
   |> Seq.map fst
   |> Seq.map (fun name -> Var(name, typeof<obj>))
   |> Seq.toList

let getRecordConstructorVar compiler (recType : System.Type) =
    let name =
        let ci = recType.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance).[0]
        let typeArgs = getGenericTypeArgs recType
        let specialization = getSpecializationString compiler typeArgs
        JavaScriptNameMapper.mapMethod ci + specialization
    compiler.DefineGlobal name (fun var ->
        let cons =
            let vars = getRecordVars recType
            let this = Var("__this", typeof<obj>)
            vars, Block
                [ yield CopyThisToVar(this)
                  for var in vars do
                    yield Assign(PropertyGet(Reference this, var.Name), Reference var) ]
        [ Assign(Reference var, Lambda cons) ])

let getCaseConsVars caseType = 
   getFields caseType
   |> Seq.map (fun (name, t) -> Var(name, typeof<obj>), t)
   |> Seq.toList

let getCaseVars (uci:UnionCaseInfo) =
   let mi, t = Quote.getCaseMethodInfo uci
   if mi.GetParameters().Length = 0 then []
   else getCaseConsVars t

let getUnionCaseConstructorVar compiler (uci : UnionCaseInfo) =
    let createConstructor uci compiler =
       let vars = getCaseVars uci |> List.map fst
       let this = Var("__this", typeof<obj>)
       vars, Block
        [ yield CopyThisToVar(this)
          yield Assign(PropertyGet(Reference this, "Tag"), Number(float uci.Tag))
          yield Assign(PropertyGet(Reference this, "_CaseName"), JSExpr.String uci.Name)
          for var in vars do yield Assign(PropertyGet(Reference this, var.Name), Reference var) ]
    let name =
       let typeArgs = getGenericTypeArgs uci.DeclaringType
       let specialization = getSpecializationString compiler typeArgs
       JavaScriptNameMapper.mapType uci.DeclaringType + "_" + uci.Name + specialization
    compiler.DefineGlobal name (fun var -> 
        [Assign(Reference var, Lambda <| createConstructor uci compiler)])

let getLambdaVars(fields : Type[]) =
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
   let argsVar, vars = getLambdaVars (fields |> Array.map (fun pi -> pi.PropertyType))
   let constructorLambda =
      Expr.Lambda(argsVar, Expr.Coerce(Expr.NewUnionCase(uci, vars), typeof<obj>))
   <@@ Core.Type.UnionCaseInfo(name, tag, %%constructorLambda, %%arrayExpr) @@>

and private getKind compiler getTypeVar t =
    if FSharpType.IsRecord t then 
        let pis = FSharpType.GetRecordFields t
        let exprs = pis |> Array.map (getPropertyInfoExpr getTypeVar) |> Array.toList
        let arrayExpr = Expr.NewArray(typeof<Core.Type.PropertyInfo>, exprs)
        let argsVar, vars = getLambdaVars (pis |> Array.map (fun pi -> pi.PropertyType))
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
        let argsVar, vars = getLambdaVars ts
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
   
   let typeArgsArrayExpr = 
      let unit = Var("unit", typeof<unit>)
      let body = Expr.NewArray(typeof<Core.Type.Type>, typeArgExprs)
      Expr.Lambda(unit, body)
   let kindExpr = 
      let unit = Var("unit", typeof<unit>)
      let body = getKind compiler getTypeVar t
      Expr.Lambda(unit, body)
   <@@ Core.Type.Type(name, fullName, %%typeArgsArrayExpr, %%kindExpr) @@>

let rec buildRuntimeType (compiler : InternalCompiler.ICompiler) (t : System.Type) =
   let typeName = sprintf "t_%s" (JavaScriptNameMapper.mapType t)
   compiler.DefineGlobal typeName (fun var ->
      let expr = netTypeExpr compiler (buildRuntimeType compiler) t
      compiler.Compile (ReturnStrategies.assignVar var) expr
   )

let components = 
   [
      [
         CompilerComponent.generateArityWithCompiler // TODO: Check this
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