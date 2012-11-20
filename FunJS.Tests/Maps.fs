[<FunJS.JS>]
module FunJS.Tests.Maps

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Map construction from lists works``() =
   check  
      <@@ 
         let xs = Map [1,1; 2,2]
         xs |> Seq.isEmpty
      @@>

[<Fact>]
let ``Map.isEmpty works``() =
   check  
      <@@ 
         let xs = Map.empty<int, int>
         xs |> Seq.isEmpty
      @@>

[<Fact>]
let ``Map.IsEmpty works``() =
   check  
      <@@ 
         let xs = Map.empty<int, int>
         xs.IsEmpty
      @@>

[<Fact>]
let ``Map.empty works``() =
   check  
      <@@ 
         let xs = Map.empty<int,int>
         ()
      @@>

[<Fact>]
let ``Map.Count works``() =
   check  
      <@@ 
         let xs = Map.empty<int, int>
         xs.Count |> float
      @@>

[<Fact>]
let ``Map.add works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1
         float xs.Count
      @@>

[<Fact>]
let ``Map.Add works``() =
   check  
      <@@ 
         let xs = Map.empty.Add(1, 1)
         float xs.Count
      @@>

[<Fact>]
let ``Map.containsKey works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1
         xs |> Map.containsKey 1
      @@>

[<Fact>]
let ``Map.ContainsKey works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1
         xs.ContainsKey 1
      @@>


[<Fact>]
let ``Map.remove works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1 |> Map.remove 1
         xs.IsEmpty
      @@>

[<Fact>]
let ``Map.Remove works``() =
   check  
      <@@ 
         let xs = (Map.empty |> Map.add 1 1).Remove 1
         xs.IsEmpty
      @@>

[<Fact>]
let ``Map.iter works``() =
   check  
      <@@ 
         let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
         let total = ref 0.
         xs |> Map.iter (fun x y -> total := !total + x + y)
         !total
      @@>


[<Fact>]
let ``Map.forAll works``() =
   check  
      <@@ 
         let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
         xs |> Map.forall (fun x y -> x < 5.)
      @@>

[<Fact>]
let ``Map.exists works``() =
   check  
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         xs |> Map.exists (fun k v -> k = 2)
      @@>

[<Fact>]
let ``Map.filter works``() =
   check  
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = xs |> Map.filter (fun x y -> x % 2 = 0)
         ys.Count |> float
      @@>

[<Fact>]
let ``Map.partition works``() =
   check  
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let ys, zs = xs |> Map.partition (fun x y -> x % 2 = 0)
         float(ys.Count + zs.Count)
      @@> 

[<Fact>]
let ``Map.fold works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         xs |> Map.fold (fun acc k v -> v + acc) 0.
      @@>

[<Fact>]
let ``Map.foldBack works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         Map.foldBack (fun k v acc -> v + acc) xs 0.
      @@>

[<Fact>]
let ``Map.map works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = xs |> Map.map (fun k v -> v * 2.)
         ys.ContainsKey 1
      @@>

[<Fact>]
let ``Map.find works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         xs |> Map.find 1
      @@>

[<Fact>]
let ``Map.tryFind works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         (xs |> Map.tryFind 0).IsNone
      @@>

[<Fact>]
let ``Map.TryFind works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         (xs.TryFind 3).IsSome
      @@>

[<Fact>]
let ``Map.tryFindKey works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         (xs |> Map.tryFindKey (fun k v -> k = 3)).IsSome
      @@>

[<Fact>]
let ``Map.pick works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let y = xs |> Map.pick (fun k v -> 
            match k with
            | 3 -> Some 10
            | _ -> None)
         float y
      @@>

[<Fact>]
let ``Map.tryPick works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let y = xs |> Map.tryPick (fun k v -> 
            match k with
            | 3 -> Some 11
            | _ -> None)
         float y.Value
      @@>

[<Fact>]
let ``Map.ofList works``() =
   check   
      <@@ 
         let xs = Map.ofList [1,1.; 2,4.; 3,9.; 4,16.]
         float xs.Count
      @@>

[<Fact>]
let ``Map.ofArray works``() =
   check   
      <@@ 
         let xs = Map.ofArray [|1,1.; 2,4.; 3,9.; 4,16.|]
         float xs.Count
      @@>

[<Fact>]
let ``Map.ofSeq works``() =
   check   
      <@@ 
         let xs = Map.ofSeq [1,1.; 2,4.; 3,9.; 4,16.]
         float xs.Count
      @@>

[<Fact>]
let ``Map.toList works``() =
   check   
      <@@ 
         let xs = [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = Map.ofList xs
         let zs = Map.toList ys
         xs = zs
      @@>

[<Fact>]
let ``Map.toArray works``() =
   check   
      <@@ 
         let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
         let ys = Map.ofArray xs
         let zs = Map.toArray ys
         xs = zs
      @@>

[<Fact>]
let ``Map.toSeq works``() =
   check   
      <@@ 
         let xs = seq [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = Map.ofSeq xs
         let zs = Map.toSeq ys
         ()
      @@>