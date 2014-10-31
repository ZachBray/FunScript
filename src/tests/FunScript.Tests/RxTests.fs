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

//[<Test>]
//let ``should translate dispose``() =
//   checkRx 
//      <@@
//         let z = ref 0.0
//         let xs = Event<float>()
//         let ys = xs.Publish
//         log "A"
//         let bs = Observable.map (fun x -> log x; x) ys 
//         log "B"
//         let subscription = Observable.subscribe(fun x -> log x; z := !z + x) bs
//         log "C"
//         xs.Trigger 1.0
//         xs.Trigger 2.0
//         xs.Trigger 3.0
//         log "D"
//         subscription.Dispose()
//         log "E"
//         xs.Trigger 4.0
//         xs.Trigger 5.0
//         log !z
//         !z
//      @@>