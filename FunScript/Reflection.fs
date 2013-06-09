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

      // Huh?
      typeof<string>.FullName
      typeof<char>.FullName
      typeof<bool>.FullName
   ]
   
let genTypeVar (t : System.Type) =
    Var.Global(sprintf "type_arg_%s" t.Name, typeof<obj>)

let isGenericParameter (t : System.Type) =
   t.IsGenericParameter

/// For generic specialization. 
let isPrimitive (t : Type) =
   primitiveTypes.Contains t.FullName

let specializeGeneric (mb : MethodBase) =
   if mb.IsGenericMethod (*|| mb.IsGenericMethodDefinition *) then
      let realArgs = mb.GetGenericArguments()
      match mb with
      | :? MethodInfo as mi -> 
         let genericMb = mi.GetGenericMethodDefinition()
         let genericArgs = genericMb.GetGenericArguments()
         let appliedArgs =
            Array.map2 (fun realT genericT ->
               //TODO: It would be nice to share "real"/JS type methods
               // but it doesn't work with typeof<'a> because it is specialized.
               if isPrimitive realT then realT, realT.Name
               else genericT, genericT.Name
            ) realArgs genericArgs
         let specialization =
            appliedArgs |> Seq.map snd |> String.concat ""
         let appliedArgs = appliedArgs |> Array.map fst
         let appliedMi = genericMb.MakeGenericMethod appliedArgs
         let genericLeftovers = appliedArgs |> Array.filter isGenericParameter
         appliedMi :> MethodBase, specialization
      | _ -> mb, ""
   else mb, ""

let private netTypeExpr (t : Type) =
   let name = t.Name
   <@ Core.Type.Type(name) @>
       
let private knownJSTypes =
   Map [
      //TODO: Might need to fill out with all representations, e.g., int, uint32...
      typeof<float>.FullName, "Number"
      typeof<string>.FullName, "String"
      typeof<char>.FullName, "Char"
      typeof<bool>.FullName, "Boolean"
   ]

let buildRuntimeType (compiler : InternalCompiler.ICompiler) (t : System.Type) =
   let typeName = sprintf "runtime_type_%s_%s" t.Namespace t.Name |> JavaScriptNameMapper.sanitize
   compiler.DefineGlobal typeName (fun var ->
      [
         let expr = netTypeExpr t
         yield! compiler.Compile (ReturnStrategies.assignVar var) expr
         match knownJSTypes |> Map.tryFind t.FullName with
         | None -> ()
         | Some jsT ->
            let jsTVar = Var(jsT, typeof<obj>)
            let getTypeMeth = PropertyGet(PropertyGet(Reference jsTVar, "prototype"), "GetType")
            let getTypeImpl = AST.Lambda([], Block [AST.Return(Reference var)])
            yield Assign(getTypeMeth, getTypeImpl)
      ]
   )

let components = 
   [
      [
         CompilerComponent.generateArityWithCompiler 
            <@ fun x -> x.GetType() @> 
            (fun _ _ (|Split|) compiler -> 
               function
               | [Split(decls, ref) as expr] ->
                  if not (isGenericParameter expr.Type) then
                     buildRuntimeType compiler expr.Type |> ignore<Var>
                  Some([decls], Apply(PropertyGet(ref, "GetType"), []))
               | _ -> None)

         CompilerComponent.generateArityWithCompiler
            <@ typeof<_> @>
            (fun _ typeArgs (|Split|) compiler ->
               function
               | [] ->
                  match typeArgs with
                  | [|t|] -> 
                     let typeVar =
                        if not(isGenericParameter t) then buildRuntimeType compiler t
                        else genTypeVar t
                     Some([], Reference typeVar)
                  | _ -> None
               | _ -> None)

      ]

      ExpressionReplacer.createTypeMethodMappings typeof<System.Type> typeof<Core.Type.Type>

   ] |> List.concat