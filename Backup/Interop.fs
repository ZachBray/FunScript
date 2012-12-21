namespace FunJS.TypeScript


open System.Reflection

type Emit() =
   static member CallImpl (isStatic:bool) (name:string) (args:obj[]):obj = 
      failwith "never"
   static member Call =
      let flags = BindingFlags.Public ||| BindingFlags.Static
      typeof<Emit>.GetMethod("CallImpl", flags)

namespace FunJS.Interop

open FunJS
open FunJS.AST
open FunJS.TypeScript

module Components =
   let private callReplacement =
      CompilerComponent.ternary <@ Emit.CallImpl @> (fun isStaticExpr objExpr argExpr ->
         match isStaticExpr, objExpr, argExpr with
         | Boolean isStatic, String meth, Array exprs -> 
            if isStatic then Apply(UnsafeReference meth, exprs)
            else Apply(PropertyGet(exprs.Head, meth), exprs.Tail)
         | _ -> failwith "Expected a string and an array")

   let all = [
      callReplacement
   ]