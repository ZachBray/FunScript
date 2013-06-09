module internal FunScript.Reflection

open FunScript.AST
open Microsoft.FSharp.Quotations
open System.Collections.Generic

let private netTypeExpr (t : System.Type) =
   let name = t.Name
   <@ Core.Type.Type(name) @>
       
let private knownTypes =
   [
      typeof<float>, "Number"
      typeof<string>, "String"
      typeof<char>, "Char"
      typeof<bool>, "Boolean"
   ]

let components = 
   [
   //ExpressionReplacer.createUnsafe <@ fun (x:int) -> x.GetType() @> (netTypeExpr<int>())
   //CompilerComponent.unary <@ fun x -> x.GetType() @> (fun arg -> PropertyGet(arg, "GetNetType()"))
      [
         CompilerComponent.generateArityWithCompiler
            <@ fun x -> x.GetType() @>
            (fun (|Split|) compiler ->
               function
               | [Split(decls, ref)] ->
                  // TODO: If we knew that we weren't inside generics here (getting the type won't tell us this
                  // as it will be whatever the first call to the generic method specifies) we could limit
                  // the types we generate. Although 4 isn't _too_ bad.
                  for t, jsT in knownTypes do
                     let typeName = sprintf "runtime_type_%s" t.FullName
                     compiler.DefineGlobal typeName (fun var ->
                        [
                           let expr = netTypeExpr t
                           yield! compiler.Compile (ReturnStrategies.assignVar var) expr
                           
                           let jsTVar = Var(jsT, typeof<obj>)
                           let getTypeMeth = PropertyGet(PropertyGet(Reference jsTVar, "prototype"), "GetType")
                           let getTypeImpl = AST.Lambda([], Block [AST.Return(Reference var)])
                           yield Assign(getTypeMeth, getTypeImpl)
                        ]
                     ) |> ignore<Var>
                  Some([decls], (Apply(PropertyGet(ref, "GetType"), [])))
               | _ -> None)

      
      ]

      ExpressionReplacer.createTypeMethodMappings typeof<System.Type> typeof<Core.Type.Type>

   ] |> List.concat