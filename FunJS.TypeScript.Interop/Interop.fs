namespace FunJS.TypeScript

open System.Reflection

type Emit() =
   static member PropertyGetImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"

   static member PropertyGet =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("PropertyGetImpl", flags)

   static member PropertySetImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"

   static member PropertySet =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("PropertySetImpl", flags)

   static member CallImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"

   static member Call =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("CallImpl", flags)

   static member NewImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"

   static member New =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("NewImpl", flags)

   static member ReplaceMethodImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"

   static member ReplaceMethod =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("ReplaceMethodImpl", flags)

   static member CreateObjectImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"

   static member CreateObject =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("CreateObjectImpl", flags)

namespace FunJS.Interop

open FunJS
open FunJS.AST
open FunJS.TypeScript
open Microsoft.FSharp.Quotations

module Components =
   let private replace quote f =
      CompilerComponent.generateArity quote (fun split -> 
         function
         | Patterns.Value(:? bool as isStatic, _)::
           Patterns.Value(:? string as meth, _):: 
           Patterns.NewArray(_, exprs)::[] -> 
            let decls, refs = exprs |> List.map split |> List.unzip
            let call = f isStatic meth refs
            match call with
            | Some(moreDecls, call) -> Some(decls @ [moreDecls], call)
            | None -> None
         | _ -> None)

   let private callReplacement =
      replace <@ Emit.CallImpl @> (fun isStatic methName args ->
         let call =
            if isStatic then Apply(UnsafeReference methName, args)
            else 
               match methName with
               | "" -> Apply(args.Head, args.Tail)
               | _ -> Apply(PropertyGet(args.Head, methName), args.Tail)
         Some([], call))

   let private newReplacement =
      replace <@ Emit.NewImpl @> (fun isStatic methName args ->
         if isStatic then Some([], New(methName, args))
         else None)

   let private getterReplacement =
      replace <@ Emit.PropertyGetImpl @> (fun isStatic propName ->
         function
         | [] when isStatic -> Some([], UnsafeReference propName)
         | [objRef] when not isStatic -> Some([], PropertyGet(objRef, propName))
         | _ -> None)

   let private setterReplacement =
      replace <@ Emit.PropertySetImpl @> (fun isStatic propName ->
         function
         | [valRef] when isStatic -> 
            Some([Assign(UnsafeReference propName, valRef)], Null)
         | [objRef; valRef] when not isStatic -> 
            Some([Assign(PropertyGet(objRef, propName), valRef)], Null)
         | _ -> None)

   let private createObjectReplacement =
      replace <@ Emit.CreateObjectImpl @> (fun isStatic propName ->
         function
         | [] when isStatic -> Some([], Object [])
         | _ -> None)

   let private replaceMethodReplacement =
      CompilerComponent.generateArityWithCompiler <@ Emit.ReplaceMethodImpl @> (fun (|Split|) compiler -> 
         function
         | Patterns.Value(:? bool as isStatic, _)::
           Patterns.Value(:? string as meth, _):: 
           Patterns.NewArray(_, [objExpr; Patterns.Coerce(lambda, _)])::[] -> 
            match objExpr, lambda with
            | Split(objDecl, objRef), DerivedPatterns.Lambdas(vars, bodyExpr) ->
               let vars = vars |> List.concat
               let replacement = Lambda(vars, Block(compiler.Compile ReturnStrategies.returnFrom bodyExpr))
               Some([objDecl; [Assign(PropertyGet(objRef, meth), replacement)]], Null)
            | _ -> None
         | _ -> None)

   let all = [
      callReplacement
      newReplacement
      replaceMethodReplacement
      getterReplacement
      setterReplacement
      createObjectReplacement
   ]