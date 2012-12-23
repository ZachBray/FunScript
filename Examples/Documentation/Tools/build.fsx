// --------------------------------------------------------------------------------------
// Builds the documentation from FSX files in the 'samples' directory
// (the documentation is stored in the 'docs' directory)
// --------------------------------------------------------------------------------------

#I "../packages/FSharp.Formatting.1.0.6/lib/net40"
#load "../packages/FSharp.Formatting.1.0.6/literate/literate.fsx"
open System.IO
open FSharp.Literate

let source = __SOURCE_DIRECTORY__
let template = Path.Combine(source, "template.html")
let tutorial = Path.Combine(source, "../Tutorial/Page.fs")
let output = Path.Combine(source, "../Documentation/output/tutorial.html")
let options = 
  "-r:\"" + Path.Combine(source, "../../FunJS/bin/Debug/FunJS.dll") + "\" " +
  "-r:\"" + Path.Combine(source, "../../FunJS.TypeScript/bin/Debug/FunJS.TypeScript.dll") + "\""

Literate.ProcessScriptFile(tutorial, template, output, compilerOptions = options)
