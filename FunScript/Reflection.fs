module internal FunScript.Reflection

open FunScript.AST
open Microsoft.FSharp.Quotations
open System
open System.Reflection
open System.Collections.Generic

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
   "_" + (
      ts |> Seq.map (fun (t : Type) ->
         //TODO: Name isn't safe. We really need to keep a dictionary around
         // to make this safer. Using only the short names if there are no collisions.
         if isPrimitive t then t.Name
         elif compiler.ShouldFlattenGenericsForReflection then JavaScriptNameMapper.mapType t
         else "obj"
      ) |> String.concat "_")
   |> JavaScriptNameMapper.sanitizeAux
   

let rec private netTypeExpr (t : Type) =
   let name = t.Name
   let fullName = t.FullName
   let typeArgs =
      let isGeneric = t.IsGenericType || t.IsGenericTypeDefinition
      if isGeneric then t.GetGenericArguments()
      else [||]
   let typeArgExprs = typeArgs |> Array.toList |> List.map netTypeExpr
   let typeArgsArrayExpr = Expr.NewArray(typeof<Core.Type.Type>, typeArgExprs)
   <@@ Core.Type.Type(name, fullName, %%typeArgsArrayExpr) @@>

let buildRuntimeType (compiler : InternalCompiler.ICompiler) (t : System.Type) =
   let typeName = sprintf "t_%s" (JavaScriptNameMapper.mapType t)
   compiler.DefineGlobal typeName (fun var ->
      let expr = netTypeExpr t
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

      ExpressionReplacer.createTypeMethodMappings typeof<System.Type> typeof<Core.Type.Type>

   ] |> List.concat