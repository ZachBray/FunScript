(*
    Modifying the HtmlCanvasExample to represent TS which populates the whole HTML page body part.
    The Project.fs then creates an index file with the HTML contents and opens it using the default brower.
*)

[<ReflectedDefinition>]
module BodyElementsExample

open FunScript
open FSharp.Data
open System.IO

// Just the paint part from the HTMLCanvasExample example
let paintCanvas (canvas: HTMLCanvasElement) =
    canvas.width <- 2000.
    canvas.height <- 1500.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- "rgb(200,0,0)"
    ctx.fillRect (10., 10., 100., 50.);
    ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
    ctx.fillRect (30., 30., 55., 100.)

// Create a function that will be compiled into JavaScript...
// Good reference for JS and HTML: http://www.w3schools.com/jsref/dom_obj_document.asp
let bodyElementsTS()=
    //Globals.window.alert("Hello world!")
    let document = Globals.document
    let body = document.body
    let h1 = document.createElement_h1()
    h1.innerHTML <- "Using HTML5 Canvas (BodyElementsExample)"
    body.appendChild(h1) |> ignore
    let canvas = Globals.document.createElement_canvas()
    body.appendChild(canvas) |> ignore
    paintCanvas canvas
    

let bodyElementsCode =
    Compiler.Compiler.Compile(
        // This argument is the quotation to compile
        <@ bodyElementsTS() @>, 
        // This argument tells the compiler not to wrap 
        // the result in a return statement
        noReturn=true)

// Here we define the page we'll create...
let bodyElementsPage = 
    sprintf """<!doctype html>
<html>
<head>
</head>
<body>
<script>
%s
</script>
</body>
</html>"""  bodyElementsCode
