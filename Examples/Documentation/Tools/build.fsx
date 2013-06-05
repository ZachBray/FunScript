// --------------------------------------------------------------------------------------
// Builds the documentation 
// --------------------------------------------------------------------------------------

#I "../../packages/FSharp.Formatting.1.0.6/lib/net40"
#load "../../packages/FSharp.Formatting.1.0.6/literate/literate.fsx"
open System.IO
open FSharp.Literate

let generateDocs () =
  let extractString tag (page:string) = 
    let startIndex = page.IndexOf("<!-- [" + tag + "] -->")
    let endIndex = page.IndexOf("<!-- [/" + tag + "] -->")
    page.Substring(startIndex, endIndex - startIndex)

  let source = __SOURCE_DIRECTORY__
  let template = Path.Combine(source, "template.html")
  let tutorial = Path.Combine(source, "../../Tutorial/Page.fs")
  let output = Path.Combine(source, "../Output/tutorial.html")
  let options = 
    "-r:\"" + Path.Combine(source, "../../../FunScript/bin/Debug/FunScript.dll") + "\" " +
    "-r:\"" + Path.Combine(source, "../../../FunScript.TypeScript.Interop/bin/Debug/FunScript.TypeScript.Interop.dll") + "\" " +
    "-r:\"" + Path.Combine(source, "../../../FunScript.TypeScript/bin/Debug/FunScript.TypeScript.dll") + "\""

  let html = Path.Combine(source, "../../Tutorial/bin/Debug/index.html")
  let page = File.ReadAllText(html)

  let js = Path.Combine(source, "../../Tutorial/bin/Debug/tutorial.js")
  let info = 
    [ "sample-javascript", File.ReadAllText(js) 
      "sample-body", extractString "body" page
      "sample-title", extractString "title" page ]

  Literate.ProcessScriptFile(tutorial, template, output, compilerOptions = options, replacements = info)

generateDocs ()