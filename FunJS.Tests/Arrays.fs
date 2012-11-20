module FunJS.Tests.Arrays

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Array literals work``() =
   check 
      <@@ 
         let x = [| 1; 2; 3; 4; 5 |]
         true
      @@>

[<Fact>]
let ``Array indexer getter works``() =
   check 
      <@@ 
         let x = [| 1.; 2.; 3.; 4.; 5. |]
         x.[2]
      @@>

[<Fact>]
let ``Array indexer setter works``() =
   check 
      <@@ 
         let x = [| 1.; 2.; 3.; 4.; 5. |]
         x.[3] <- 10.
      @@>

[<Fact>]
let ``Array.Length works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         xs.Length |> float
      @@>

[<Fact>]
let ``Array.zeroCreate works``() =
   check  
      <@@ 
         let xs = Array.zeroCreate 2
         // length doesn't seem to work properly in Jint
         // float xs.Length
         ()
      @@>

[<Fact>]
let ``Array.blit works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = Array.zeroCreate 2
         Array.blit xs 2 ys 0 2
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.copy works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = Array.copy xs
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.sub works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = Array.sub xs 2 2
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.fill works``() =
   check  
      <@@ 
         let xs = Array.zeroCreate 2
         Array.fill xs 0 2 2.
         xs.[0] + xs.[1]
      @@>

[<Fact>]
let ``Array.empty works``() =
   check  
      <@@ 
         let xs = Array.empty<int>
         float xs.Length
      @@>

[<Fact>]
let ``Array.append works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = [|0.|]
         let zs = Array.append ys xs
         zs.[0] + zs.[1]
      @@>

[<Fact>]
let ``Array.average works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         Array.average xs
      @@>

[<Fact>]
let ``Array.averageBy works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         Array.averageBy ((*) 2.) xs
      @@>

[<Fact>]
let ``Array.choose works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let result = xs |> Array.choose (fun x ->
            if x > 2. then Some x
            else None) 
         result.[0] + result.[1]
      @@>

[<Fact>]
let ``Array.collect works``() =
   check  
      <@@ 
         let xs = [|[|1.|]; [|2.|]; [|3.|]; [|4.|]|]
         let ys = xs |> Array.collect id
         ys.[0]  + ys.[1]
      @@>

let ``Array.concat works``() =
   check  
      <@@ 
         let xs = [|[|1.|]; [|2.|]; [|3.|]; [|4.|]|]
         let ys = xs |> Array.concat
         ys.[0]  + ys.[1]
      @@>

[<Fact>]
let ``Array.exists works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         xs |> Array.exists (fun x -> x = 2.)
      @@>

[<Fact>]
let ``Array.exists2 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = [|1.; 2.; 3.; 4.|]
         Array.exists2 (fun x y -> x * y = 16.) xs ys
      @@>

[<Fact>]
let ``Array.filter works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = xs |> Array.filter (fun x -> x > 2.)
         float ys.Length
      @@>

[<Fact>]
let ``Array.find works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         xs |> Array.find ((=) 2.)
      @@>

[<Fact>]
let ``Array.findIndex works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         xs |> Array.findIndex ((=) 2.)
         |> float
      @@>

[<Fact>]
let ``Array.fold works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let total = xs |> Array.fold (+) 0.
         total
      @@>

[<Fact>]
let ``Array.fold2 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = [|1.; 2.; 3.; 4.|]
         let total = Array.fold2 (fun x y z -> x + y + z) 0. xs ys
         total
      @@>

[<Fact>]
let ``Array.foldBack works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let total = Array.foldBack (+) xs 0.
         total
      @@>

[<Fact>]
let ``Array.foldBack2 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = [|1.; 2.; 3.; 4.|]
         let total = Array.foldBack2 (fun x y z -> x + y + z) xs ys 0.
         total
      @@>

[<Fact>]
let ``Array.forall works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         Array.forall (fun x -> x < 5.) xs
      @@>

[<Fact>]
let ``Array.forall2 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = [|1.; 2.; 3.; 4.|]
         Array.forall2 (=) xs ys
      @@>

[<Fact>]
let ``Array.init works``() =
   check  
      <@@ 
         let xs = Array.init 4 float
         xs.[0] + xs.[1]
      @@>

[<Fact>]
let ``Array.isEmpty works``() =
   check  
      <@@ 
         let xs = [|1|]
         Array.isEmpty xs
      @@>

[<Fact>]
let ``Array.iter works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let total = ref 0.
         xs |> Array.iter (fun x ->
            total := !total + x
         )
         !total
      @@>

[<Fact>]
let ``Array.iter2 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let total = ref 0.
         Array.iter2 (fun x y ->
            total := !total + x + y
         ) xs xs
         !total
      @@>

[<Fact>]
let ``Array.iteri works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let total = ref 0.
         xs |> Array.iteri (fun i x ->
            total := !total + (float i) * x
         )
         !total
      @@>

[<Fact>]
let ``Array.iteri2 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let total = ref 0.
         Array.iteri2 (fun i x y ->
            total := !total + (float i) * x + (float i) * y
         ) xs xs
         !total
      @@>

[<Fact>]
let ``Array.length works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         Array.length xs 
         |> float
      @@>

[<Fact>]
let ``Array.map works``() =
   check  
      <@@ 
         let xs = [|1.|]
         let ys = xs |> Array.map ((*) 2.)
         ys.[0]
      @@>

[<Fact>]
let ``Array.map2 works``() =
   check  
      <@@ 
         let xs = [|1.|]
         let ys = [|2.|]
         let zs = Array.map2 (*) xs ys
         zs.[0]
      @@>

[<Fact>]
let ``Array.mapi works``() =
   check  
      <@@ 
         let xs = [|1.|]
         let ys = xs |> Array.mapi (fun i x -> float i + x)
         ys.[0]
      @@>

[<Fact>]
let ``Array.mapi2 works``() =
   check  
      <@@ 
         let xs = [|1.|]
         let ys = [|2.|]
         let zs = Array.mapi2 (fun i x y -> float i + x * y) xs ys
         zs.[0]
      @@>

[<Fact>]
let ``Array.max works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.max
      @@>

[<Fact>]
let ``Array.maxBy works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.maxBy (fun x -> -x)
      @@>

[<Fact>]
let ``Array.min works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.min
      @@>

[<Fact>]
let ``Array.minBy works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.minBy (fun x -> -x)
      @@>

[<Fact>]
let ``Array.ofList works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = Array.ofList xs
         ys.[0]
      @@>

[<Fact>]
let ``Array.ofSeq works``() =
   check  
      <@@ 
         let xs = [1.; 2.] :> _ seq
         let ys = Array.ofSeq xs
         ys.[0]
      @@>

[<Fact>]
let ``Array.partition works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys, zs = xs |> Array.partition (fun x -> x <= 1.)
         ys.[0] - zs.[0]
      @@>

[<Fact>]
let ``Array.permute works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = xs |> Array.permute (fun i -> i + 1 - 2 * (i % 2))
         ys.[0]
      @@>

[<Fact>]
let ``Array.pick works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.pick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
      @@>

[<Fact>]
let ``Array.reduce works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.reduce (+)
      @@>

[<Fact>]
let ``Array.reduceBack works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.reduceBack (+)
      @@>

[<Fact>]
let ``Array.rev works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = xs |> Array.rev
         ys.[0]
      @@>

[<Fact>]
let ``Array.scan works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = xs |> Array.scan (+) 0.
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.scanBack works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.|]
         let ys = Array.scanBack (+) xs 0.
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.sort works``() =
   check  
      <@@ 
         let xs = [|3.; 4.; 1.; 2.|]
         let ys = xs |> Array.sort
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.sortBy works``() =
   check  
      <@@ 
         let xs = [|3.; 4.; 1.; 2.|]
         let ys = xs |> Array.sortBy (fun x -> -x)
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.sortWith works``() =
   check  
      <@@ 
         let xs = [|3.; 4.; 1.; 2.|]
         let ys = xs |> Array.sortWith (fun x y -> int(x - y))
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.sortInPlace works``() =
   check  
      <@@ 
         let xs = [|3.; 4.; 1.; 2.|]
         Array.sortInPlace xs
         xs.[0] + xs.[1]
      @@>

[<Fact>]
let ``Array.sortInPlaceBy works``() =
   check  
      <@@ 
         let xs = [|3.; 4.; 1.; 2.|]
         Array.sortInPlaceBy (fun x -> -x) xs
         xs.[0] + xs.[1]
      @@>

[<Fact>]
let ``Array.sortInPlaceWith works``() =
   check  
      <@@ 
         let xs = [|3.; 4.; 1.; 2.|]
         Array.sortInPlaceWith (fun x y -> int(x - y)) xs
         xs.[0] + xs.[1]
      @@>

[<Fact>]
let ``Array.sum works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.sum
      @@>

[<Fact>]
let ``Array.sumBy works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         xs |> Array.sumBy ((*) 2.)
      @@>

[<Fact>]
let ``Array.toList works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = xs |> Array.toList
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Array.toSeq works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = xs |> Array.toSeq
         ys |> Seq.head
      @@>

[<Fact>]
let ``Array.tryFind works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = xs |> Array.tryFind ((=) 1.)
         ys.IsSome
      @@>

[<Fact>]
let ``Array.tryFindIndex works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = xs |> Array.tryFindIndex ((=) 2.)
         ys.Value |> float
      @@>

[<Fact>]
let ``Array.tryPick works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let r = xs |> Array.tryPick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
         match r with
         | Some x -> x
         | None -> 0.
      @@>

[<Fact>]
let ``Array.unzip works``() =
   check  
      <@@ 
         let xs = [|1., 2.|]
         let ys, zs = xs |> Array.unzip
         ys.[0] + zs.[0]
      @@>

[<Fact>]
let ``Array.unzip3 works``() =
   check  
      <@@ 
         let xs = [|1., 2., 3.|]
         let ys, zs, ks = xs |> Array.unzip3
         ys.[0] + zs.[0] + ks.[0]
      @@>

[<Fact>]
let ``Array.zip works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.|]
         let ys = [|1.; 2.; 3.|]
         let zs = Array.zip xs ys
         let x, y = zs.[0]
         x + y
      @@>

[<Fact>]
let ``Array.zip3 works``() =
   check  
      <@@ 
         let xs = [|1.; 2.; 3.|]
         let ys = [|1.; 2.; 3.|]
         let zs = [|1.; 2.; 3.|]
         let ks = Array.zip3 xs ys zs
         let x, y, z = ks.[0]
         x + y + z
      @@>

