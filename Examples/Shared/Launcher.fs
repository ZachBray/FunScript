namespace FunScript

open System.IO
open System.Net
open System.Threading
open System.Text

// ----------------------------------------------------------------------------
// Simple web server hosting static files
// ----------------------------------------------------------------------------

module RuntimeImplementation = 

  [<AutoOpen>]
  module HttpExtensions = 

    type System.Net.HttpListener with
      member x.AsyncGetContext() = 
        Async.FromBeginEnd(x.BeginGetContext, x.EndGetContext)

    type System.Net.HttpListenerRequest with
      member request.InputString =
        use sr = new StreamReader(request.InputStream)
        sr.ReadToEnd()

    type System.Net.HttpListenerResponse with
      member response.Reply(s:string) = 
        let buffer = Encoding.UTF8.GetBytes(s)
        response.ContentLength64 <- int64 buffer.Length
        response.OutputStream.Write(buffer,0,buffer.Length)
        response.OutputStream.Close()
      member response.Reply(typ, buffer:byte[]) = 
        response.ContentLength64 <- int64 buffer.Length
        response.ContentType <- typ
        response.OutputStream.Write(buffer,0,buffer.Length)
        response.OutputStream.Close()

  /// Simple HTTP server
  type HttpServer private (url, root) =

    let contentTypes = dict [ ".css", "text/css"; ".html", "text/html"; ".js", "text/javascript" ]
    let tokenSource = new CancellationTokenSource()
  
    let agent = MailboxProcessor<HttpListenerContext>.Start((fun inbox -> async { 
      while true do
        let! context = inbox.Receive()
        let s = context.Request.Url.LocalPath 

        // Handle an ordinary file request
        let file = root + (if s = "/" then "/index.html" else s)
        if File.Exists(file) then 
          let ext = Path.GetExtension(file).ToLower()
          let typ = contentTypes.[ext]
          context.Response.Reply(typ, File.ReadAllBytes(file))
        else 
          context.Response.Reply(sprintf "File not found: %s" file) }), tokenSource.Token)

    let server = async { 
      use listener = new HttpListener()
      listener.Prefixes.Add(url)
      listener.Start()
      while true do 
        let! context = listener.AsyncGetContext()
        agent.Post(context) }

    do Async.Start(server, cancellationToken = tokenSource.Token)

    /// Stops the HTTP server and releases the TCP connection
    member x.Stop() = tokenSource.Cancel()

    /// Starts new HTTP server on the specified URL. The specified
    /// function represents computation running inside the agent.
    static member Start(url, root) = 
      new HttpServer(url, root)

// ----------------------------------------------------------------------------
// Main method that finds 'main' function and generates JS file
// ----------------------------------------------------------------------------

open FunScript
open System.IO
open System.Reflection
open Microsoft.FSharp.Quotations
open RuntimeImplementation

type Runtime private() = 
  static member Run(?expression, ?port, ?directory, ?components, ?browse) =
    let components = defaultArg components []
    let port = defaultArg port 8081
    let directory = defaultArg directory ""
    let browse = defaultArg browse true

    // Find the main method in this assembly
    let thisAsm = Assembly.GetExecutingAssembly()
    let main =
      match expression with 
      | None ->
          printfn "Searching for main function..."
          let types = thisAsm.GetTypes()
          let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
          let mains = 
            [ for typ in types do
                for mi in typ.GetMethods(flags) do
                  if mi.Name = "main" then yield mi ]
          let main = 
            match mains with
            | [it] -> it
            | _ -> failwith "Main function not found!"
          printfn "Found entry point..."
          Expr.Call(main, [])
      | Some e -> e

    // Get the path
    let root = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    let root = Path.Combine(root, directory)

    // Compile the main function into a script
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let source = FunScript.Compiler.Compiler.Compile(main, components=components)
    let sourceWrapped = sprintf "$(document).ready(function () {\n%s\n});" source
    let filename = Path.Combine(root, "page.js")
    printfn "Generated JavaScript in %f sec..." (float sw.ElapsedMilliseconds / 1000.0) 
    System.IO.File.Delete filename
    System.IO.File.WriteAllText(filename, sourceWrapped)

    // Starting the web server
    let url = sprintf "http://localhost:%d/" port
    let server = HttpServer.Start(url, root)
    printfn "Starting web server at %s" url
    if browse then 
      System.Diagnostics.Process.Start(url) |> ignore

    System.Console.ReadLine() |> ignore
    server.Stop()