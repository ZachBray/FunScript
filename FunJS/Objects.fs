module (*internal*) FunJS.Objects

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System

let private isUnionOrRecord t =
   let isUnionOrRecord t = 
      FSharpType.IsUnion t || FSharpType.IsRecord t
   let result = 
      isUnionOrRecord t || (
         t.IsGenericType && 
         isUnionOrRecord <| t.GetGenericTypeDefinition()
      )
   result  

let private propertyGetter =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.PropertyGet(Some(Split(objDecl, objRef)), pi, [])
            //when pi.DeclaringType |> isUnionOrRecord 
            ->
         [ yield! objDecl 
           yield returnStategy.Return <| PropertyGet(objRef, JavaScriptNameMapper.sanitize pi.Name)
         ]
      | _ -> []

let private propertySetter =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.PropertySet(Some(Split(objDecl, objRef)), pi, [], Split(valDecl, valRef))
            //when pi.DeclaringType |> isUnionOrRecord 
            ->
         [ yield! objDecl 
           yield! valDecl
           yield Assign(PropertyGet(objRef, JavaScriptNameMapper.sanitize pi.Name), valRef)
           if returnStategy = ReturnStrategies.inplace then
               yield returnStategy.Return Null 
         ]
      | _ -> []


let private localized (name:string) =
   let sections = name.Split '-'
   JavaScriptNameMapper.sanitize sections.[sections.Length - 1]

let private getAllMethods (t:System.Type) =
   t.GetMethods(
      BindingFlags.Public ||| 
      BindingFlags.NonPublic ||| 
      BindingFlags.FlattenHierarchy ||| 
      BindingFlags.Instance)

let methodCallPattern (mb:MethodBase) =
   let argCounts = mb.GetCustomAttribute<CompilationArgumentCountsAttribute>()
   match Expr.TryGetReflectedDefinition mb with
   | Some (DerivedPatterns.Lambdas(vars, bodyExpr)) 
      when argCounts <> Unchecked.defaultof<_>
           && mb.IsStatic ->
      let argCounts = argCounts.Counts |> Seq.toList
      let varsAndConstructors =
         List.map2 (fun (vars:Var list) count ->
            if vars.Length = count then
               vars, None
            elif vars.Length = 1 then
               let var = vars.Head
               let genericArgs = var.Type.GetGenericArguments()
               let subVars =
                  [  for i = 0 to count - 1 do
                        yield Var(sprintf "%s_%i" var.Name i, genericArgs.[i], var.IsMutable)
                  ]
               let refs = subVars |> List.map Expr.Var
               let construction = Expr.NewTuple(refs)
               subVars, Some (var, construction) 
            else failwith "Unexpected argument format"               
            ) vars argCounts
      let vars  = varsAndConstructors |> List.collect fst
      let constructions = varsAndConstructors |> List.choose snd
      let bodyExpr =
         constructions |> List.fold (fun acc (var, value) ->
            Expr.Let(var, value, acc)) bodyExpr
      Some(vars, bodyExpr)
   | Some (DerivedPatterns.Lambdas(vars, bodyExpr)) ->
      Some(vars |> List.concat, bodyExpr)
   | Some expr ->
      Some([], expr)
   | None -> None

let (|CallPattern|_|) = methodCallPattern

let getInstanceMethods t =
   getAllMethods t
   |> Array.choose (fun mi ->
      match mi with
      | CallPattern(objVar::vars, bodyExpr) ->
         let this = Var("this", objVar.Type, objVar.IsMutable)
         let objVar, exprWithoutThis =
            if objVar.Name = "this" then
               let replacementThis = Var("__", objVar.Type, objVar.IsMutable)
               replacementThis, bodyExpr.Substitute(function
                  | v when v = objVar -> Some <| Expr.Var replacementThis
                  | _ -> None)
            else objVar, bodyExpr
         let updatedBodyExpr = 
            Expr.Let(objVar, Expr.Var this, exprWithoutThis)
         Some(localized mi.Name, vars, updatedBodyExpr)
      | _ -> None)
   |> Seq.distinctBy (fun (name, _, _) -> name)
   |> Seq.toArray

let private hasIComparableImplementation (t:System.Type) =
   t.GetInterfaces() 
   |> Array.exists (fun t -> t.Name = typeof<IComparable>.Name)

let getFields (t:Type) =
   t.GetProperties(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
   |> Seq.map (fun p -> p, p.GetCustomAttribute<CompilationMappingAttribute>())
   |> Seq.filter (fun (p, attr) -> not <| obj.ReferenceEquals(null, attr)) 
   |> Seq.filter (fun (p, attr) -> SourceConstructFlags.Field = attr.SourceConstructFlags)
   |> Seq.sortBy (fun (p, attr) -> attr.SequenceNumber)
   |> Seq.map (fun (p, attr) -> JavaScriptNameMapper.sanitize p.Name, p.PropertyType)
   |> Seq.toList

let genComparisonFunc t =
   let fields = getFields t
   let that = Var("that", typeof<obj>)
   let diff = Var("diff", typeof<obj>)
      
   let body =
      List.foldBack (fun (name, t) acc ->
         let thisField = PropertyGet(This, name)
         let thatField = PropertyGet(Reference that, name)
         let compareExpr = Comparison.compareCall thisField thatField
         [  Assign(Reference diff, compareExpr)
            IfThenElse(
               BinaryOp(Reference diff, "!=", Number 0.),
               Block [ Return <| Reference diff ],
               Block acc)
         ]) fields [ Return <| Number 0. ]
      
   Lambda(
      [that],
      Block <| Declare [diff] :: body
   )

let genComparisonMethods t =
   let isGeneratedCompareRequired = 
      isUnionOrRecord t && hasIComparableImplementation t
   if not isGeneratedCompareRequired then []
   else
      let func = genComparisonFunc t
      [ Assign(PropertyGet(This, "CompareTo"), func) ]
      
let genInstanceMethods t (compiler:InternalCompiler.ICompiler) =
   let methods = getInstanceMethods t
   let comparisonMethods = genComparisonMethods t
   [  for name, vars, bodyExpr in methods do
         let methodRef = PropertyGet(This, name) 
         let body = compiler.Compile ReturnStrategies.returnFrom bodyExpr
         yield Assign(methodRef, Lambda(vars, Block body)) 
      yield! comparisonMethods
   ]

let components = [ 
   propertyGetter
   propertySetter
]