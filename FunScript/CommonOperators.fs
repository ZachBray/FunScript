module internal FunScript.CommonOperators

open AST
open System.Reflection
open Microsoft.FSharp.Quotations

[<Inline; JS>]
module private Replacements =

   let ref x = { contents = x }
   let op_Bang (r:_ Ref) = r.contents
   let assign (r: _ Ref) v = r.contents <- v

   let pipe x f = f x
   let pipe2 (x, y) f = f x y
   let pipe3 (x, y, z) f = f x y z

   let pipeBack f x = f x
   let pipeBack2 f (x, y) = f x y
   let pipeBack3 f (x, y, z) = f x y z

   let compose f g : 'a -> 'b = fun x -> g(f x)
   let composeBack f g : 'a -> 'b = fun x -> f(g x)
   
   let ignore _ = ()

   let defaultArg x y = match x with None -> y | Some v -> v

   let incr (x:int ref): unit = x := !x + 1
   let decr (x:int ref): unit = x := !x - 1

   let applyCurried2 curriedFunc arg1 arg2 =
      Apply(Apply(curriedFunc, 
                  [arg1]),
                  [arg2])

   let applyCurried3 curriedFunc arg1 arg2 arg3 =
      Apply(Apply(Apply(curriedFunc, 
                        [arg1]),
                        [arg2]),
                        [arg3])

let private defaultValue =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.DefaultValue _ -> [ returnStategy.Return Null ]
      | _ -> []

let private coerce =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Coerce(expr, t) -> 
         if expr.Type = t || t = typeof<obj> || (expr.Type.IsInterface && t.IsAssignableFrom expr.Type) then 
            compiler.Compile returnStrategy expr
         elif t.IsInterface then
            let actualType = expr.Type
            let targetMethods =
               [
                  for i in ExpressionReplacer.getInterfaces t do
                     let mapping = actualType.GetInterfaceMap i
                     yield! mapping.TargetMethods
               ]
            let members =
               targetMethods
               |> Seq.map (fun realMi -> 
                  let replacementMi = Objects.replaceIfAvailable compiler realMi Quote.CallType.MethodCall
                  Objects.localized replacementMi.Name, replacementMi)
               |> Seq.groupBy fst
               |> Seq.map (fun (name, mis) ->
                  name, mis |> Seq.tryPick (snd >> Objects.methodCallPattern))
               |> Seq.toArray
            let hasAllMembers =
               members |> Array.forall (snd >> Option.isSome)
            if hasAllMembers then
               let impl = Var("impl", typeof<obj>)
               let members = 
                  members
                  |> Array.map (fun (name, expr) -> name, Option.get expr)
                  |> Array.map (fun (name, (vars, lambdaExpr)) ->
                     let objVar = List.head vars
                     let vars = List.tail vars
                     name,
                     Lambda(
                        vars,
                        Block [
                           Return(
                              Apply(
                                 Lambda(
                                    objVar::vars,
                                    Block(compiler.Compile ReturnStrategies.returnFrom lambdaExpr)),
                                 (impl :: vars) |> List.map Reference))
                        ]))
                  |> Array.toList
               [
                  yield Declare [impl]
                  yield! compiler.Compile (ReturnStrategies.assignVar impl) expr
                  yield returnStrategy.Return(Object members)
               ]
            else compiler.Compile returnStrategy expr
         else compiler.Compile returnStrategy expr

         // For strictness when testing:
         //   else []
         // else []
      | _ -> []

let private tryCatch =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
   function
   | Patterns.TryFinally(tryExpr, finallyExpr) ->
      let tryStmts = compiler.Compile returnStategy tryExpr
      let finallyStmts = compiler.Compile ReturnStrategies.inplace finallyExpr
      [ TryFinally(Block tryStmts, Block finallyStmts) ]
   | _ -> []
 
// TODO: Implement this properly. Through type property?
let private typeTest =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
   function
   | Patterns.TypeTest(_, _) ->
      [ returnStategy.Return <| Boolean false ]
   | _ -> []

let components = 
   [
      [
         // Refs
         ExpressionReplacer.create <@ ref @> <@ Replacements.ref @>
         ExpressionReplacer.create <@ (!) @> <@ Replacements.op_Bang @>
         ExpressionReplacer.create <@ (:=) @> <@ Replacements.assign @>
   
         // Piping
         ExpressionReplacer.create <@ (|>) @> <@ Replacements.pipe @>
         ExpressionReplacer.create <@ (||>) @> <@ Replacements.pipe2 @>
         ExpressionReplacer.create <@ (|||>) @> <@ Replacements.pipe3 @>
         ExpressionReplacer.create <@ (<|) @> <@ Replacements.pipeBack @>
         ExpressionReplacer.create <@ (<||) @> <@ Replacements.pipeBack2 @>
         ExpressionReplacer.create <@ (<|||) @> <@ Replacements.pipeBack3 @>
         ExpressionReplacer.create <@ (>>) @> <@ Replacements.compose @>
         ExpressionReplacer.create <@ (<<) @> <@ Replacements.composeBack @>

         // Funcs
         ExpressionReplacer.create <@ ignore @> <@ Replacements.ignore @>
         ExpressionReplacer.create <@ defaultArg @> <@ Replacements.defaultArg @>
         CompilerComponent.unary <@ id @> id
         ExpressionReplacer.create <@ incr @> <@ Replacements.incr @>
         ExpressionReplacer.create <@ decr @> <@ Replacements.decr @>

         // Conversions
         CompilerComponent.unary <@ sbyte @> id
         CompilerComponent.unary <@ byte @> id
         CompilerComponent.unary <@ int16 @> id
         CompilerComponent.unary <@ uint16 @> id
         CompilerComponent.unary <@ int @> id
         CompilerComponent.unary <@ int32 @> id
         CompilerComponent.unary <@ uint32 @> id
         CompilerComponent.unary <@ int64 @> id
         CompilerComponent.unary <@ uint64 @> id
         CompilerComponent.unary <@ float @> id
         CompilerComponent.unary <@ single @> id
         CompilerComponent.unary <@ float32 @> id
         CompilerComponent.unary <@ double @> id
         CompilerComponent.unary <@ fun x -> x.ToString() @> (fun expr -> Apply(PropertyGet(expr, "toString"),[])) 
         ExpressionReplacer.create <@ char @> <@ FunScript.Core.String.FromCharCode @>
          
         // Seq + ranges
         CompilerComponent.unary <@ seq @> id
         ExpressionReplacer.create <@ op_Range @> <@ FunScript.Core.Range.oneStep @>
         ExpressionReplacer.create <@ op_RangeStep @> <@ FunScript.Core.Range.customStep @>

         // Casting
         coerce
         typeTest
         CompilerComponent.unary <@ box @> id
         CompilerComponent.unary <@ unbox @> id
         CompilerComponent.unary <@ InternalCompiler.Helpers.Cast @> id

         // Exns 
         CompilerComponent.unaryStatement <@ raise @> Throw
         CompilerComponent.unaryStatement <@ invalidOp @> Throw
         CompilerComponent.unaryStatement <@ failwith @> Throw
         CompilerComponent.binaryStatement <@ invalidArg @> (fun field msg -> Throw msg)
         tryCatch

         // Default values
         CompilerComponent.nullary <@ Unchecked.defaultof<_> @> Null
         defaultValue


         // OptimizedClosures
         CompilerComponent.unary 
            <@ OptimizedClosures.FSharpFunc<_,_,_>.Adapt @> id
         CompilerComponent.ternary 
            <@ fun (f:OptimizedClosures.FSharpFunc<_,_,_>) x y -> f.Invoke(x,y) @> 
            Replacements.applyCurried2

         CompilerComponent.unary 
            <@ OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt @> id 
         CompilerComponent.nary4
            <@ fun (f:OptimizedClosures.FSharpFunc<_,_,_,_>) x y z -> f.Invoke(x,y,z) @> 
            Replacements.applyCurried3
      ] 
      
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
         "FunScript" "FunScript.Core.LanguagePrimitives"

   ] |> List.concat