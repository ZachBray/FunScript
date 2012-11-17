module internal FunJS.Objects

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection

let private isUnionOrRecord t =
   FSharpType.IsUnion t || FSharpType.IsRecord t

let private propertyGetter =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.PropertyGet(Some(Split(objDecl, objRef)), pi, [])
            when pi.DeclaringType |> isUnionOrRecord ->
         [ yield! objDecl 
           yield returnStategy.Return <| PropertyGet(objRef, pi.Name)
         ]
      | _ -> []

let private propertySetter =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.PropertySet(Some(Split(objDecl, objRef)), pi, [], Split(valDecl, valRef))
            when pi.DeclaringType |> isUnionOrRecord ->
         [ yield! objDecl 
           yield! valDecl
           yield Assign(PropertyGet(objRef, pi.Name), valRef)
           if returnStategy = ReturnStrategies.inplace then
               yield returnStategy.Return Null 
         ]
      | _ -> []


let private localized (name:string) =
   let sections = name.Split '-'
   sections.[sections.Length - 1]

let getInstanceMethods (t:System.Type) =
   t.GetMethods(
      BindingFlags.Public ||| 
      BindingFlags.NonPublic ||| 
      BindingFlags.FlattenHierarchy ||| 
      BindingFlags.Instance)
   |> Array.choose (fun mi ->
      Expr.TryGetReflectedDefinition mi 
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

let genInstanceMethods t (compiler:InternalCompiler.ICompiler) =
   let methods = getInstanceMethods t
   [  for name, vars, bodyExpr in methods do
         let methodRef = PropertyGet(This, name) 
         let body = compiler.Compile ReturnStrategies.returnFrom bodyExpr
         yield Assign(methodRef, Lambda(vars, Block body)) ]

let components = [ 
   propertyGetter
   propertySetter
]