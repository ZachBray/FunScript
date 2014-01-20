<<<<<<< HEAD
(*
Main module. 
============
Intended as a reference project for beginners who aim to get up and running and learn the basics.
Moreover, intended to focus on examples leveraging FunScript for Mobile development, especially with
JQueryMobile and PhoneGap.

Used with MS Visual Studio 2013 (professional), 
with the vanilla "FunScript - Not Hosted" project template (by Zach Bary), 
which can be found through the online project search. Plus relevant FunScript binding packages (through NuGet).

Your todo:
REPLACE the "FunScript - Not Hosted" Project.fs file contents with this file.
ADD relevant FunScript.TypeScript.Binding.? packages 
    Through NuGet manager for your solution: FunScript.TypeScript.Binding.jquerymobile and FunScript.TypeScript.Binding.phonegap
ADD the rest of the examples *.fs files to the visual studio project. NOTICE: file order matters for MS VS.
SELECT below which example page you want to write into the index file.

Leveraging some examples from the Funscript GitHub directory:
https://github.com/ZachBray/FunScript/tree/master/Examples
Such as the Canvas example (by Zach Bary) - offered as a good first project (hello-world alternative) here.

Adding examples for JQueryMobile and PhoneGap (TBD).
*)

=======
>>>>>>> Create Program.fs
[<ReflectedDefinition>]
module Program

open FunScript
open FSharp.Data
open System.IO

<<<<<<< HEAD
// Each file contains a different example
open HtmlCanvasExample          // Zach Bary's example, the FS TS code draws 2 rectangles and inject them into the html file that does the rest
open BodyElementsExample        // Modifying the HtmlCanvasExample to represent TS which populates the whole HTML body
open JQueryMobileExample        // Modifying the BodyElementsExample to work with the JQueryMobile library.
open Mazing                     // Simple "maze" mobile application, leveraging DOM, js, jquerymobile, cordova/phonegap

// Create a file...
let filePath = Path.Combine(System.Environment.CurrentDirectory, "index.html")

// Write the page
// ------------------------------------------
// SELECT which example you would like to use
// ------------------------------------------
//File.WriteAllText(filePath, htmlCanvasPage)
//File.WriteAllText(filePath, bodyElementsPage)
//File.WriteAllText(filePath, jQueryMobilePage)
File.WriteAllText(filePath, mazingPage)

// Open the file in the default web browser...
=======
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
>>>>>>> Create Program.fs
System.Diagnostics.Process.Start(filePath) |> ignore
