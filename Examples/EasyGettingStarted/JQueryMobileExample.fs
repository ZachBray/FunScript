(*
    Modifying the BodyElementsExample to work with the JQueryMobile library.
    The Project.fs then creates an index file with the HTML contents and opens it using the default brower.
*)

[<ReflectedDefinition>]
module JQueryMobileExample

open FunScript
open FSharp.Data
open System.IO

// Just the paint part from the HTMLCanvasExample example
let paintCanvas (canvas: HTMLCanvasElement) =
    canvas.width <- 200.
    canvas.height <- 200.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- "rgb(200,0,0)"
    ctx.fillRect (10., 80., 100., 50.);
    ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
    ctx.fillRect (10., 10., 55., 100.)

// Create a function that will be compiled into JavaScript...
// See at the bottom of the file the equivalent HTML body XML structure 
// which this TS is creating by code
// Good reference for JS and HTML: http://www.w3schools.com/jsref/dom_obj_document.asp
let jQueryMobileTS()=
    let document = Globals.document
    let body = document.body

    let divPage = document.createElement_div()
    divPage.setAttribute ("data-role", "page")
    divPage.setAttribute ("id", "mainpage")

    let divHeader = document.createElement_div()
    divHeader.setAttribute ("data-role", "header")
    let divHeaderH1 = document.createElement_h1()
    divHeaderH1.innerHTML <- "Header: Using HTML5 Canvas (JQueryMobileExample)"
    divHeader.appendChild(divHeaderH1) |> ignore

    let divContent = document.createElement_div()
    divContent.setAttribute ("data-role", "content")
    let divContentH1 = document.createElement_h1()
    divContentH1.innerHTML <- "Content ..."
    divContent.appendChild(divContentH1) |> ignore
    // Paint canvas
    let canvas = Globals.document.createElement_canvas()
    divContent.appendChild(canvas) |> ignore
    paintCanvas canvas

    let divFooter = document.createElement_div()
    divFooter.setAttribute ("data-role", "footer")
    let divFooterH1 = document.createElement_h1()
    divFooterH1.innerHTML <- "Footer"
    divFooter.appendChild(divFooterH1) |> ignore

    divPage.appendChild(divHeader) |> ignore
    divPage.appendChild(divContent) |> ignore
    divPage.appendChild(divFooter) |> ignore

    body.appendChild(divPage) |> ignore    

let jQueryMobileCode =
    Compiler.Compiler.Compile(
        // This argument is the quotation to compile
        <@ jQueryMobileTS() @>, 
        // This argument tells the compiler not to wrap 
        // the result in a return statement
        noReturn=true)

// Here we define the page we'll create...
let jQueryMobilePage = 
    sprintf """<!doctype html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Mazing</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.css">
    <script src="http://code.jquery.com/jquery-1.8.3.min.js"></script>
    <script src="http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.js"></script>
</head>
<body>
<script>
%s
</script>
</body>
</html>"""  jQueryMobileCode

// For documentation purposes only ...
// XML structure equivalent (excluding the canvas) to the TS which describes the inside of the HTML body
(*
    <div data-role="page" id="pageone">
      <div data-role="header">
        <h1>Insert Page Title Here</h1>
      </div>

      <div data-role="content">
        <p>Insert Content Here</p>
      </div>

      <div data-role="footer">
        <h1>Insert Footer Text Here</h1>
      </div>
    </div> 
*)
