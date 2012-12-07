[<ReflectedDefinition>]
module Program

open FunJS

[<JSEmit("alert({0});")>]
let alert (x:obj): unit = failwith "never"

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
    // Hello World
    "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." 
    |> run
    |> alert

// Compile
let source = <@@ main() @@> |> Compiler.compileWithoutReturn 
let filename = "brainfuck.js"
System.IO.File.Delete filename
System.IO.File.WriteAllText(filename, source)
source|> printfn "%A"
System.Console.ReadLine() |> ignore