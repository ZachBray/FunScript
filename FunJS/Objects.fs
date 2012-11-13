module internal FunJS.Objects

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

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

let components = [ 
   propertyGetter
   propertySetter
]