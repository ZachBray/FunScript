[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.RxTests

open NUnit.Framework
open FunScript.Rx

[<Test>]
let ``should translate event construction``() =
   checkRx 
      <@@
         let xs = Event<float>()
         0.0
      @@>

[<Test>]
let ``should translate event subscription``() =
   checkRx 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let subscription = xs.Publish |> Observable.subscribe(fun x -> y := !y + x)
         !y
      @@>

[<Test>]
let ``should translate event triggering``() =
   checkRx 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let subscription = xs.Publish |> Observable.subscribe(fun x -> y := !y + x)
         xs.Trigger 1.0
         xs.Trigger 2.0
         !y
      @@>

[<Test>]
let ``should translate take``() =
   checkRx 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let subscription = xs.Publish 
                            |> Observable.take 3
                            |> Observable.subscribe(fun x -> y := !y + x)
         xs.Trigger 1.0
         xs.Trigger 2.0
         xs.Trigger 3.0
         xs.Trigger 4.0
         !y
      @@>

[<Test>]
let ``should translate count``() =
   checkRx 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let count = xs.Publish
                     |> Observable.take 3
                     |> Observable.count
                     |> Observable.subscribe (fun x -> y := float x)
         xs.Trigger 1.0
         xs.Trigger 2.0
         xs.Trigger 3.0
         xs.Trigger 4.0
         !y
      @@>

[<Test>]
let ``should translate map``() =
   checkRx 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let count = xs.Publish
                     |> Observable.map (fun x -> x * x * x)
                     |> Observable.subscribe (fun x -> y := float x)
         xs.Trigger 1.0
         xs.Trigger 2.0
         xs.Trigger 3.0
         xs.Trigger 4.0
         !y
      @@>

[<Test>]
let ``should translate combineLatestArray``() =
   checkRx 
      <@@
         let y = ref ""
         let xs = Event<string>()
         let ys = Event<string>()
         let count = Observable.combineLatestArray [|xs.Publish; ys.Publish|]
                     |> Observable.subscribe (fun xs ->
                        y := !y + "--" + (xs |> String.concat ","))
         xs.Trigger "A"
         xs.Trigger "B"
         ys.Trigger "1"
         ys.Trigger "2"
         xs.Trigger "C"
         xs.Trigger "D"
         ys.Trigger "3"
         ys.Trigger "4"
         !y
      @@>

[<Test>]
let ``should translate merge``() =
   checkRx 
      <@@
         let z = ref ""
         let xs = Event<string>()
         let ys = Event<string>()
         let subscription = xs.Publish 
                            |> Observable.merge ys.Publish
                            |> Observable.subscribe(fun x -> z := !z + x)
         xs.Trigger "X"
         xs.Trigger "X"
         ys.Trigger "Y"
         xs.Trigger "X"
         ys.Trigger "Y"
         !z
      @@>

[<Test>]
let ``should translate dispose``() =
   checkRx 
      <@@
         let z = ref 0.0
         let xs = Event<float>()
         let ys = xs.Publish
         let bs = Observable.map (fun x -> x * x) ys 
         let subscription = Observable.subscribe(fun x -> z := !z + x) bs
         xs.Trigger 1.0
         xs.Trigger 2.0
         xs.Trigger 3.0
         subscription.Dispose()
         xs.Trigger 4.0
         xs.Trigger 5.0
         !z
      @@>

type Action = System.Action

[<Test>]
let ``should translate create``() =
   checkRx 
      <@@
            let z = ref ""
            let append str =
                z := !z + "." + str

            let rs =
                Observable.create(fun observer ->
                    observer.OnNext "R"
                    Action(fun () -> 
                       append "R Disposed"))

            let xs =
                Observable.create(fun observer ->
                    observer.OnNext "X"
                    Action(fun () -> 
                       append "X Disposed"))

            let ys =
                Observable.create(fun observer ->
                    observer.OnNext "Y1"
                    observer.OnNext "Y2"
                    observer.OnCompleted()
                    Action(fun () -> 
                       append "Y Disposed"))

            let zs =
                Observable.create(fun observer ->
                    observer.OnNext "Z1"
                    observer.OnNext "Z2"
                    observer.OnError(exn "Z ERROR")
                    Action(fun () -> 
                       append "Z Disposed"))

            let xsSubscription = 
                xs |> Observable.subscribeWithCallbacks
                    append (fun ex -> append ex.Message) (fun () -> append "X Complete")

            let ysSubscription = 
                ys |> Observable.subscribeWithCallbacks
                    append (fun ex -> append ex.Message) (fun () -> append "Y Complete")

            let zsSubscription =
                zs |> Observable.subscribeWithCallbacks
                    append (fun ex -> append ex.Message) (fun () -> append "Z Complete") 

            xsSubscription.Dispose()
            ysSubscription.Dispose()
            zsSubscription.Dispose()

            !z
      @@>

[<FunScript.JS>]
type ActionDisposable(f) =
    interface System.IDisposable with
        member __.Dispose() = f()

[<Test>]
let ``should translate createWithDisposable``() =
   checkRx 
      <@@
            let z = ref ""
            let append str =
                z := !z + "." + str

            let rs =
                Observable.createWithDisposable(fun observer ->
                    observer.OnNext "R"
                    upcast new ActionDisposable(fun () -> 
                       append "R Disposed"))

            let xs =
                Observable.createWithDisposable(fun observer ->
                    observer.OnNext "X"
                    upcast new ActionDisposable(fun () -> 
                       append "X Disposed"))

            let ys =
                Observable.createWithDisposable(fun observer ->
                    observer.OnNext "Y1"
                    observer.OnNext "Y2"
                    observer.OnCompleted()
                    upcast new ActionDisposable(fun () -> 
                       append "Y Disposed"))

            let zs =
                Observable.createWithDisposable(fun observer ->
                    observer.OnNext "Z1"
                    observer.OnNext "Z2"
                    observer.OnError(exn "Z ERROR")
                    upcast new ActionDisposable(fun () -> 
                       append "Z Disposed"))

            let xsSubscription = 
                xs |> Observable.subscribeWithCallbacks
                    append (fun ex -> append ex.Message) (fun () -> append "X Complete")

            let ysSubscription = 
                ys |> Observable.subscribeWithCallbacks
                    append (fun ex -> append ex.Message) (fun () -> append "Y Complete")

            let zsSubscription =
                zs |> Observable.subscribeWithCallbacks
                    append (fun ex -> append ex.Message) (fun () -> append "Z Complete") 

            xsSubscription.Dispose()
            ysSubscription.Dispose()
            zsSubscription.Dispose()

            !z
      @@>