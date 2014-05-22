// ---
// header: Canvas
// tagline: Using HTML5 canvas
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript

let main() =
   let canvas = Globals.document.getElementsByTagName_canvas().[0]
   canvas.width <- 1000.
   canvas.height <- 800.
   let ctx = canvas.getContext_2d()
   ctx.fillStyle <- "rgb(200,0,0)"
   ctx.fillRect (10., 10., 55., 50.);
   ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
   ctx.fillRect (30., 30., 55., 50.)
   
do Runtime.Run(directory="Web")