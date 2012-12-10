[<ReflectedDefinition>]
module Program

open FunJS

type Dom = FunJS.TypeScript.Api< @"..\..\Examples\Typings\lib.d.ts" >

[<JSEmit("alert({0});")>]
let alert (x:obj): unit = failwith "never"

// Manual overrides until the typescript type provider supports inheritance
[<JSEmit("return {0}.value;")>]
let getValue element = failwith "never"

[<JSEmit("{0}.value += {1};")>]
let output element value : unit = failwith "never"  

let run (program:string) =
    let s = ref ""
    let b = Array.create 30000 0uy
    let p, pc = ref 0, ref 0
    let execute () =
        match program.[!pc] with
        | '>' -> incr p; incr pc
        | '<' -> decr p; incr pc
        | '+' -> b.[!p] <- b.[!p] + 1uy; incr pc
        | '-' -> b.[!p] <- b.[!p] - 1uy; incr pc
        | '.' -> s := !s + (char b.[!p]).ToString(); incr pc
        | ',' -> b.[!p] <- failwith "Not implemented"
        | '[' -> 
            if b.[!p] = 0uy then while program.[!pc] <> ']' do incr pc
            else incr pc
        | ']' ->
            if b.[!p] <> 0uy then while program.[!pc] <> '[' do decr pc
            else incr pc
        | _ -> incr pc
    while !pc < program.Length do execute ()
    !s



let main() =

    let txtCode = Dom.document.getElementById("code")    
    let submit = Dom.document.getElementById("execute_code")
    let console = Dom.document.getElementById("output")

    let executeCode (e:Dom.MouseEvent') =        
        txtCode |> getValue |> run |> output console
        ignore |> box

    submit.onclick <- executeCode

// Compile
let source = <@@ main() @@> |> Compiler.compileWithoutReturn 
let filename = "brainfuck.js"
System.IO.File.Delete filename
System.IO.File.WriteAllText(filename, source)
source|> printfn "%A"
System.Console.ReadLine() |> ignore