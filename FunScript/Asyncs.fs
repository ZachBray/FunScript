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
         ExpressionReplacer.createUnsafe <@ Async.StartImmediate @> <@ FunAsync.StartImmediate @>
         ExpressionReplacer.createUnsafe <@ Async.Sleep @> <@ FunAsync.Sleep @>
         ExpressionReplacer.createUnsafe <@ Async.FromContinuations @> <@ FunAsync.FromContinuations @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.Bind @> <@ fun (a:FunAsyncBuilder) -> a.Bind @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.Delay @> <@ fun (a:FunAsyncBuilder) -> a.Delay @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.Return @> <@ fun (a:FunAsyncBuilder) -> a.Return @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.ReturnFrom @> <@ fun (a:FunAsyncBuilder) -> a.ReturnFrom @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.While @> <@ fun (a:FunAsyncBuilder) -> a.While @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.For @> <@ fun (a:FunAsyncBuilder) -> a.For @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.Combine @> <@ fun (a:FunAsyncBuilder) -> a.Combine @>
         ExpressionReplacer.createUnsafe <@ fun (a:AsyncBuilder) -> a.Zero @> <@ fun (a:FunAsyncBuilder) -> a.Zero @>
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
   ] |> List.concat