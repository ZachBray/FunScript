(*
    Based on Zach Bary's "Canavas" example (see GitHub examples).
    The TS draws 2 rectangles and we inject the compiled JS code into an HTML page.
    The Project.fs then creates an index file with the HTML contents and opens it using the default brower.

    Recommended as your first FunScript example (hello world alternative)
*)

[<ReflectedDefinition>]
module HtmlCanvasExample

open FunScript
open FSharp.Data
open System.IO

// Create a function that will be compiled into JavaScript...
let htmlCanvasTS()=
    let canvas = Globals.document.getElementsByTagName_canvas().[0]
    canvas.width <- 1000.
    canvas.height <- 800.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- "rgb(200,0,0)"
    ctx.fillRect (10., 10., 55., 50.);
    ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
    ctx.fillRect (30., 30., 55., 50.)

let htmlCanvasCode =
    Compiler.Compiler.Compile(
        // This argument is the quotation to compile
        <@ htmlCanvasTS() @>, 
        // This argument tells the compiler not to wrap 
        // the result in a return statement
        noReturn=true)

// Here we define the page we'll create...
let htmlCanvasPage = 
    sprintf """<!doctype html>
<html>
<head>
</head>
<body>
        <h1><!-- [title] -->Using HTML5 Canvas (HtmlCanvasExample)<!-- [/title] --></h1>
  
  <!-- [body] -->
  <canvas align="center" id="canvas" width="1000" height="800"></canvas>
  <!-- [/body] -->
<script>
%s
</script>
</body>
</html>"""  htmlCanvasCode
