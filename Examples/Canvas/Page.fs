// ---
// header: Canvas
// tagline: Using HTML5 canvas
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript

type ts = Api<"../Typings/lib.d.ts">

let main() =
   let canvas = unbox<ts.HTMLCanvasElement>(ts.document.getElementById("canvas"))
   canvas.width <- 1000.
   canvas.height <- 800.
   let ctx = canvas.getContext("2d")
   ctx.fillStyle <- "rgb(200,0,0)"
   ctx.fillRect (10., 10., 55., 50.);
   ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
   ctx.fillRect (30., 30., 55., 50.)
   

do Runtime.Run(directory="Web", components=Interop.Components.all)