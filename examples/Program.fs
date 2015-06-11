[<ReflectedDefinition>]
module Program

open FunScript
open FSharp.Data
open System.IO

// Create a function that will be compiled into JavaScript...
let main () =
    let canvas = Globals.document.getElementsByTagName_canvas().[0]
    canvas.width <- 1000.
    canvas.height <- 800.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- "rgb(200,0,0)"
    ctx.fillRect (10., 10., 55., 50.);
    ctx.fillStyle <- "rgba(0, 0, 200, 0.5)"
    ctx.fillRect (30., 30., 55., 50.)

// Compile the main() function into javascript code...
let code =
    Compiler.Compiler.Compile(
        // This argument is the quotation to compile
        <@ main() @>, 
        // This argument tells the compiler not to wrap 
        // the result in a return statement
        noReturn=true//, 
        // This tells the compiler about the additional components 
        // that are needed to compile the code. In this case,
        // these are the components that provide mappings for the
        // FSharp.Data type providers (incl. the WorldBank provider).
        //components = FunScript.Data.Components.DataProviders  
        )

// Here we define the page we'll create...
// Note: You have to manually include references to the JavaScript dependencies.
let page = sprintf """<!doctype html>
<html>
<head>
  <script src="http://code.jquery.com/jquery-1.8.0.js"></script>
  <script type="text/javascript" src="page.js"></script>
  <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
</head>
<body>
        <h1><!-- [title] -->Using HTML5 Canvas<!-- [/title] --></h1>
  
  <!-- [body] -->
  <canvas align="center" id="canvas" width="1000" height="800"></canvas>
  <!-- [/body] -->
<script>
%s
</script>
</body>
</html>"""  code

// We write the page to a file...
let filePath = Path.Combine(System.Environment.CurrentDirectory, "index.html")
File.WriteAllText(filePath, page)
// We open the file in the default web browser...
System.Diagnostics.Process.Start(filePath) |> ignore
