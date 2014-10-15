[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.EventTests

open NUnit.Framework

[<Test>]
let ``should translate event construction``() =
   check 
      <@@
         let xs = Event<float>()
         0.0
      @@>

[<Test>]
let ``should translate event subscription``() =
   check 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let subscription = xs.Publish |> Observable.subscribe(fun x -> y := !y + x)
         !y
      @@>

[<Test>]
let ``should translate event triggering``() =
   check 
      <@@
         let y = ref 0.0
         let xs = Event<float>()
         let subscription = xs.Publish |> Observable.subscribe(fun x -> y := !y + x)
         xs.Trigger 1.0
         xs.Trigger 2.0
         !y
      @@>