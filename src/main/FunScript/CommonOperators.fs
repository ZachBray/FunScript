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

   let exn(msg : string) = Core.LanguagePrimitives.Exception(msg)

// TODO: Specialize for ints/floats etc. to give 0. as in .NET
let private defaultValue =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.DefaultValue _ -> [ returnStategy.Return Null ]
      | _ -> []

// TODO: Refactor!!!
let private coerce =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Coerce(expr, t) ->
         if expr.Type.IsGenericType && expr.Type.GetGenericTypeDefinition() = typedefof<_ System.Collections.Generic.IList> then
            let elementT = expr.Type.GetGenericArguments().[0]
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ seq> then
                let mi, _ = Quote.toMethodInfoFromLambdas <@ Array.toSeq @>
                let mi = mi.GetGenericMethodDefinition().MakeGenericMethod [|elementT|]
                let unboxedExpr = Expr.Coerce(expr, mi.GetParameters().[0].ParameterType)
                let fixedExpr = Expr.Call(mi, [unboxedExpr])
                compiler.Compile returnStrategy fixedExpr
            elif (t.IsArray && t.GetElementType() = elementT) || t = typeof<obj> then
                compiler.Compile returnStrategy expr
            else []
         elif expr.Type = t 
            || t = typeof<obj> 
            || (expr.Type.IsInterface && t.IsAssignableFrom expr.Type)
            || (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ System.Collections.Generic.IList>) then 
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
                  |> Array.map (fun (name, getVarsExpr) ->
                     let vars, lambdaExpr = getVarsExpr()
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

open Reflection
let private getPrimaryConstructorName compiler (t: System.Type): string =
    let cons = t.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance).[0]
    JavaScriptNameMapper.mapMethod cons + (Reflection.getSpecializationString compiler <| Reflection.getGenericMethodArgs cons)

// TODO: Add tests
let private typeTest =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
   function
   | Patterns.TypeTest(expr, t) ->
      match expr with
      | Patterns.Var var ->
          let returnTypeTest operator jsTarget =
            [ returnStrategy.Return <| BinaryOp(Reference var, operator, jsTarget) ]
          
          // F# compiler won't allow anything not boxed to be tested against obj
          // so if this test has been allowed it will always hold true
          if t = typeof<obj> then
            [ returnStrategy.Return <| Boolean true ]

          // Primitives
          elif jsNumberTypes.Contains t.FullName || t.IsEnum then
            returnTypeTest "typeof" (String "number")
          elif jsStringTypes.Contains t.FullName then
            returnTypeTest "typeof" (String "string")
          elif t = typeof<bool> then
            returnTypeTest "typeof" (String "boolean")
          elif t = typeof<System.DateTime> then
            returnTypeTest "instanceof" (String "Date")
          
          // Interfaces
          elif t.IsInterface then
            // TODO: Implement
            // TODO: Check if interface comes from JS library
            []
          
          // Objects
          else
            // TODO: Check if the constructor has already been defined in JS
            // TODO: Check if there's reflected definition of constructor
            // TODO: Support inheritance (recursively check for type of "base" JS property)

            let cons = getPrimaryConstructorName compiler t
            returnTypeTest "instanceof" (EmitExpr (fun _ -> cons))

            // TODO: Do sth with Array and ResizeArray?

       | _ -> [ returnStrategy.Return <| Boolean false ]
   | _ -> [ returnStrategy.Return <| Boolean false ]

let components = 
   [
      [
         // Refs
         ExpressionReplacer.create <@ ref @> <@ Replacements.ref @>
         ExpressionReplacer.create <@ (!) @> <@ Replacements.op_Bang @>
         ExpressionReplacer.create <@ fun (x : _ Ref) -> x.Value @> <@ Replacements.op_Bang @>
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
         CompilerComponent.unary <@ fun x -> string x @> (fun expr -> Apply(PropertyGet(expr, "toString"),[])) 
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
         ExpressionReplacer.createUnsafe <@ fun str -> exn str @> <@ Replacements.exn @>
         CompilerComponent.unaryStatement <@ raise @> Throw
         CompilerComponent.unaryStatement <@ invalidOp @> Throw
         CompilerComponent.unaryStatement <@ failwith @> Throw
         CompilerComponent.binaryStatement <@ invalidArg @> (fun field msg -> Throw msg)

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
      
      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Exception>
         typeof<Core.LanguagePrimitives.Exception>

      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
         "FunScript" "FunScript.Core.LanguagePrimitives"

   ] |> List.concat