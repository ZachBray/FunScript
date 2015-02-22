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
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.PropertyGet(Some(Split(objDecl, objRef)), pi, [])
            when isAvailable compiler (pi.GetGetMethod(true))
            ->
         [ yield! objDecl 
           yield returnStrategy.Return <| PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux pi.Name)
         ]
      | _ -> []

let private propertySetter =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.PropertySet(Some(Split(objDecl, objRef)), pi, [], Split(valDecl, valRef))
            when isAvailable compiler (pi.GetSetMethod(true))
            ->
         [ yield! objDecl 
           yield! valDecl
           yield Assign(PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux pi.Name), valRef)
           if returnStrategy = ReturnStrategies.inplace then
               yield returnStrategy.Return Null 
         ]
      | _ -> []


let components = [ 
   propertyGetter
   propertySetter
]