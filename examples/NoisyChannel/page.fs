[<ReflectedDefinition>]
module Program

open FunScript
let j (selector : string) = Globals.Dollar.Invoke(selector)
let (?) jq name = jq("#" + name)
let hello () =  Globals.window.alert("Hello world!")

type Async =
  static member AwaitJQueryEvent(f : ('T -> obj) -> JQuery) : Async<'T> = 
    Async.FromContinuations(fun (cont, econt, ccont) ->
      let named = ref None
      named := Some (f (fun v -> 
        (!named).Value.off() |> ignore
        cont v
        "evil" :> obj )))

  static member AwaitHTMLEvent(f : ('T -> obj) -> unit) : Async<'T> = 
    Async.FromContinuations(fun (cont, econt, ccont) -> f (fun v -> cont v;obj()))

let paintInCol (ctx:CanvasRenderingContext2D) = 
   let fCol = ref 0.
   fun (imgs : ImageData array) -> 
       let fStartY =  ref 0.
       for img in imgs do
         ctx.putImageData(img,!fCol,!fStartY)
         fStartY := ! fStartY + img.height + 10.

       let maxW = imgs |> Array.map(fun i -> i.width) |> Array.max
       fCol := !fCol + maxW + 5.
       ()

[<JS; JSEmit("return Math.random();")>]
let random () : float = failwith "never"

let BSC p (sxs:bool array) = sxs |> Array.map(fun e -> if random() > p then e else let v = random() in v < 0.5 )

let allon  (sxs:bool array) =
   sxs |> Array.map(fun e -> true )


type ImageData with 
   member x.to1Bit () = 
             let gamma, ar =  2.2, x.data
             Array.init (int(ar.length / 4.))
                                (fun i ->  let l = 0.2126*pown (ar.[4*i]/255.) 2 + 0.7152*pown (ar.[4*i+1]/255.) 2 + 0.0722*pown (ar.[4*i+2]/255.) 2
                                           if l > 0.5 then false else true)
   member x.from1Bit (ar:bool array) = 
       let r = x.data
       ar |> Array.iteri(fun i b ->  if  b then r.[4*i] <- 0.  ;r.[4*i+1] <- 0.  ;r.[4*i+2] <- 0.
                                           else r.[4*i] <- 255.;r.[4*i+1] <- 255.;r.[4*i+2] <- 255.
                                     r.[4*i+3] <- 255.)
       x
 
     
let main() =
   let res = ref None
   let imgBits (src:string) = 
         Async.FromContinuations(fun (cont,econt,ccont) -> 
                                    let img = Globals.document.createElement_img()
                                    img.onload <- (fun _ -> let c = Globals.document.createElement_canvas()
                                                            let ctx = c.getContext_2d()                   
                                                            ctx.drawImage(img,0.,0.);
                                                            cont (ctx.getImageData(0.,0.,img.width,img.height))
                                                            obj())
                                    img.src <- src)
                                                                              
   let fNoise = BSC 0.4

   let rec repetitionEncoder () =
      async { let! d = imgBits  "redundant.png"
              let canvas, displaystack, newImg = 
                 let canvas = Globals.document.getElementsByTagName_canvas().[0]
                 let ctx = canvas.getContext_2d()
                 canvas, paintInCol ctx, ctx.createImageData


              let elem = j?nRepetition
              let n = elem._val() :?> float
              canvas.width <-  max canvas.width  (n*(d.width+10.))
              canvas.height <- max canvas.height (n*(d.height+5.))

              let decodedf (args: bool array array) =
                 if args.Length = 0 then failwith "pb"
                 let ac0 = Array.create ((args.[0]).Length) 0.
                 let votes, n = args |> Array.fold(fun (acVote,n) aNew -> Array.map2 (fun av e -> (n*av + if e then 1. else 0.) /(n+1.)) acVote aNew, n+1.)
                                                  (ac0,0.) 
                 votes |> Array.map ((<) 0.5)
              let original    = d.to1Bit()
              let copies      = Array.init (int n) (fun _ -> Array.copy original )
              let transmitted = (fun copies -> Array.map (Array.copy >> fNoise) copies) copies
              let decoded     = decodedf transmitted

              displaystack [| (newImg d).from1Bit(original) |]
              displaystack (copies      |> Array.map (fun data -> (newImg d).from1Bit(data)))
              let! v = Async.AwaitJQueryEvent(fun f -> j?next.click(f))

              displaystack (transmitted |> Array.map (fun data -> (newImg d).from1Bit(data)))
              let! v = Async.AwaitJQueryEvent(fun f -> j?next.click(f))
           
              displaystack [| (newImg d).from1Bit(decoded);|]
              let! v = Async.AwaitJQueryEvent(fun f -> j?next.click(f))

              do! repetitionEncoder () 
              } 
          
          
   async {
       do! Async.AwaitJQueryEvent(j?document.ready)
       Async.StartImmediate(repetitionEncoder ())
     } |> Async.StartImmediate
    


   ()


do Runtime.Run(directory="Web")
