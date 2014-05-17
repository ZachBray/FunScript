module internal FunScript.Asyncs

open AST
open System.Threading
open Microsoft.FSharp.Quotations

type FunAsync = Core.Async.Async
type FunAsyncBuilder = Core.Async.AsyncBuilder
type FunCancelTok = Core.Async.CancellationToken
type FunCancelSrc = Core.Async.CancellationTokenSource

let components = 
   [
      [
         ExpressionReplacer.createUnsafe <@ fun (a:CancellationToken) -> a.ThrowIfCancellationRequested @> <@ fun (a:FunCancelTok) -> a.ThrowIfCancellationRequested @>
         ExpressionReplacer.createUnsafe <@ fun (a:CancellationTokenSource) -> a.Cancel @> <@ fun (a:FunCancelSrc) -> a.Cancel @>
         ExpressionReplacer.createUnsafe <@ fun (a:CancellationTokenSource) -> a.Token @> <@ fun (a:FunCancelSrc) -> a.Token @>
         ExpressionReplacer.createUnsafe <@ async @> <@ Core.Async.async @>

         CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
            function
            | Patterns.NewObject(ci, args) when ci.DeclaringType = typeof<CancellationTokenSource> ->
               compiler.Compile returnStategy <@ new Core.Async.CancellationTokenSource() @>
            | _ -> []

      ]
     
      ExpressionReplacer.createTypeMethodMappings
         typeof<Microsoft.FSharp.Control.AsyncBuilder>
         typeof<Core.Async.AsyncBuilder>
         
      ExpressionReplacer.createTypeMethodMappings
         typeof<Microsoft.FSharp.Control.Async>
         typeof<Core.Async.Async>

   ] |> List.concat