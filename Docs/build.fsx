// --------------------------------------------------------------------------------------
// Builds the documentation 
// --------------------------------------------------------------------------------------

#I "../ThirdParty/FSharp.Formatting/lib/net40"
#load "../ThirdParty/FSharp.Formatting/literate/literate.fsx"
open System.IO
open System.Text.RegularExpressions
open FSharp.Literate

/// Concantenate path using the right separator
let (++) p1 p2 = Path.Combine(p1, p2)

/// Delete directory if it exists
let SafeDeleteDir directory recurse = 
  if Directory.Exists(directory) then Directory.Delete(directory, recurse)

/// Ensure that a given directory exists
let rec EnsureDirectory directory = 
  if Directory.Exists(directory) |> not then 
    EnsureDirectory (Path.GetDirectoryName(directory))
    Directory.CreateDirectory(directory) |> ignore

/// Copy files recursively and ensure all directories are created
let rec CopyFiles source target = 
  EnsureDirectory target
  for dir in Directory.GetDirectories(source) do
    CopyFiles dir (target ++ Path.GetFileName(dir))
  for file in Directory.GetFiles(source) do
    File.Copy(file, target ++ Path.GetFileName(file))

/// Extract string (from HTML page) enclosed between 
/// <!-- [tag] --> and <!-- [/tag] --> 
let extractMarkedPagePart tag (page:string) = 
  let pattern =  """\<\!--\s*\[""" + tag + """\]\s*--\>(.*)\<\!--\s*\[/""" + tag + """\]\s*--\>"""
  let mtch = Regex.Match(page, pattern, RegexOptions.Singleline)
  mtch.Groups.[1].Value

/// Search for Jekyll-style header. File can start with "---" delimited block
/// that contains "key: value" pairs. Returns dictionary with key-value 
/// pairs, together with the body. If the lines start with "//", that is ignored
let parseHeader (lines:string[]) = 
  let trimComment (line:string) =
    let line = line.Trim()
    if line.StartsWith("//") then line.Substring(2).Trim() else line
  if (trimComment lines.[0]) = "---" then
    let endIndex = lines |> Seq.skip 1 |> Seq.findIndex (fun l -> trimComment l = "---")
    let header = lines.[1 .. endIndex]
    let body = lines.[endIndex + 2 .. lines.Length - 1]
    let header =
      [ for line in header do 
          let line = trimComment line
          let split = line.IndexOf(':')
          let key = line.Substring(0, split).Trim()
          let value = line.Substring(split + 1, line.Length - split - 1).Trim()
          yield key, value ]
    header, body |> String.concat "\n"
  else 
    [], lines |> String.concat "\n"

/// Take the header from a given file and copy the rest to a temp file
let getTempFileAndHeader file = 
  let header, body = parseHeader(File.ReadAllLines(file))
  let temp = Path.GetDirectoryName(file) ++ (Path.GetFileNameWithoutExtension(file) + "~" + Path.GetExtension(file))
  File.WriteAllText(temp, body)
  header, temp
            
// --------------------------------------------------------------------------------------
// Configuration
// --------------------------------------------------------------------------------------

//let root = @"file:///C:\Tomas\Projects\FunScript\Docs\output"
let root = "http://tpetricek.github.io/FunScript"

let source = __SOURCE_DIRECTORY__     // Root directory with pages
let outputPath = source ++ "output"   // Where to save generated output
let staticFiles = source ++ "static"  // Copy all files from here to output
let funScriptRoot = source ++ "../"   // Root with FunScript projects

/// Command line options needed when type-checking sample FunScript projects
let funScriptReferences = 
  "-r:\"" + (funScriptRoot ++ "FunScript/bin/Debug/FunScript.dll") + "\" " +
  "-r:\"" + (funScriptRoot ++ "FunScript.TypeScript.Interop/bin/Debug/FunScript.TypeScript.Interop.dll") + "\" " +
  "-r:\"" + (funScriptRoot ++ "FunScript.TypeScript/bin/Debug/FunScript.TypeScript.dll") + "\" " +
  "-r:\"" + (funScriptRoot ++ "ThirdParty/FSharp.Data.dll") + "\" " +
  "-r:\"" + (funScriptRoot ++ "ThirdParty/FSharp.Data.Experimental.dll") + "\" " +
  "-r:\"" + (funScriptRoot ++ "FunScript.Data/bin/Debug/FunScript.Data.dll") + "\""

let pageTemplate = source ++ "template" ++ "page-template.html"     // General page template
let sampleTemplate = source ++ "template" ++ "sample-template.html" // Sample with live preview

// TODO: Add more samples here!!
//   We assume that each sample has 'Page.fs' file together with
//   output 'bin/Debug/index.html' and 'bin/Debug/page.js'
let samples = 
  [ "Tutorial", funScriptRoot ++ "Examples/Tutorial/", ""
    "WorldBank", funScriptRoot ++ "Examples/WorldBank/", "Web"
    "MovieDatabase", funScriptRoot ++ "Examples/MovieDatabase/", "Web"
    "Canvas", funScriptRoot ++ "Examples/Canvas/", "Web"
    "SimpleAsync", funScriptRoot ++ "Examples/SimpleAsync/", "" ]

// --------------------------------------------------------------------------------------
// Build the FunScript web site
// --------------------------------------------------------------------------------------

let generateDocs () =
  // Clean the output directory & copy static files
  SafeDeleteDir outputPath true
  CopyFiles staticFiles outputPath
  EnsureDirectory (outputPath ++ "samples")

  // Process all Markdown files in the source directory
  for doc in Directory.GetFiles(source, "*.md") do
    printfn " - processing page: %s" (Path.GetFileName(doc))
    let headers, tempInputDoc = getTempFileAndHeader doc
    Literate.ProcessMarkdown
      ( tempInputDoc, pageTemplate, outputPath ++ (Path.GetFileNameWithoutExtension(doc) + ".html"),
        lineNumbers = false, compilerOptions = funScriptReferences, 
        replacements = [ "root", root ] @ headers)
    File.Delete(tempInputDoc)

  // Generate HTML for all listed samples 
  for name, sampleSource, webDir in samples do
    printfn " - processing sample: %s" name

    // Find input & output files, generate HTML replacements
    let outputFile = outputPath ++ "samples" ++ (name.ToLower().Replace(' ', '-') + ".html")
    let headers, tempInputPage = getTempFileAndHeader (sampleSource ++ "Page.fs")

    let subdir = "bin/Debug" + (if webDir <> "" then "/" + webDir else "")
    let inputHtml = sampleSource ++ subdir ++ "index.html" |> File.ReadAllText
    let inputJs = sampleSource ++ subdir ++ "page.js" |> File.ReadAllText

    let headersDict = dict headers
    let info = 
      [ "sample-javascript", inputJs
        "root", root
        "sample-body", extractMarkedPagePart "body" inputHtml
        "sample-title", extractMarkedPagePart "title" inputHtml ]
      @ headers

    // Process the file and delete the temp
    Literate.ProcessScriptFile
      ( tempInputPage, sampleTemplate, outputFile, 
        compilerOptions = funScriptReferences, replacements = info)
    File.Delete(tempInputPage)

// --------------------------------------------------------------------------------------
// Run me :-)
// --------------------------------------------------------------------------------------

generateDocs ()