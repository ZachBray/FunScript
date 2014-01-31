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

[<ReflectedDefinition>]
module Program

open FunScript
open FSharp.Data
open System.IO

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
System.Diagnostics.Process.Start(filePath) |> ignore