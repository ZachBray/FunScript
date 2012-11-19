module internal FunJS.Objects

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

let getInstanceMethods t =
   getAllMethods t
   |> Array.choose (fun mi ->
      let reflectedDef = Expr.TryGetReflectedDefinition mi 
      reflectedDef
      |> Option.map (fun expr ->
         match expr with
         | Patterns.Lambda(var, DerivedPatterns.Lambdas(vars, bodyExpr)) ->
            let replacementThis = Var("this", var.Type, var.IsMutable)
            let updatedBodyExpr = bodyExpr.Substitute(function
               | v when v = var -> Some <| Expr.Var replacementThis
               | _ -> None)
            localized mi.Name, vars |> List.concat, updatedBodyExpr
         | _ -> failwith "expected a lambda"))
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

let genComparisonMethods t =
   let isGeneratedCompareRequired = 
      isUnionOrRecord t && hasIComparableImplementation t
   if not isGeneratedCompareRequired then []
   else
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
      [
         Assign(
            PropertyGet(This, "CompareTo"), 
            Lambda(
               [that],
               Block <| Declare [diff] :: body
            ))
      ]

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