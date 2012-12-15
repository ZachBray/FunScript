[<ReflectedDefinition>]
module Program

open FunJS
open System
open FunJS.Core.Events

type Dom = FunJS.TypeScript.Api< @"..\..\Examples\Typings\lib.d.ts" >

[<JSEmit("alert({0});")>]
let alert (x:obj): unit = failwith "never"

// Manual overrides until the typescript type provider supports inheritance
[<JSEmit("return {0}.value;")>]
let getValue element = failwith "never"

[<JSEmit("{0}.value += {1};")>]
let output element value : unit = failwith "never"  

//[<FunJS.JSEmit("return {0};")>]
//let private stringToByte (str:string) : byte = failwith "never"

let run (program:string) (inputChar: Async<byte>) (outputChar:string -> unit) =    
    //let s = ref ""
    let b = Array.create 30000 0uy
    let p, pc = ref 0, ref 0
    async {
        while !pc < program.Length do
            match program.[!pc] with
            | '>' -> incr p; incr pc
            | '<' -> decr p; incr pc
            | '+' -> b.[!p] <- b.[!p] + 1uy; incr pc
            | '-' -> b.[!p] <- b.[!p] - 1uy; incr pc
            | '.' -> outputChar ((char b.[!p]).ToString()); incr pc
            | ',' ->
                let! nextChar = inputChar
                b.[!p] <- nextChar
                incr pc
            | '[' -> 
                if b.[!p] = 0uy then while program.[!pc] <> ']' do incr pc
                else incr pc
            | ']' ->
                if b.[!p] <> 0uy then while program.[!pc] <> '[' do decr pc
                else incr pc
            | _ -> incr pc
    }

[<FunJS.JSEmit("{0}.addEventListener({1},{2});")>]
let private addEventListener (element:obj) (eventName:string) (handler:'T -> unit) : unit = failwith "never"

[<FunJS.JSEmit("{0}.removeEventListener({1},{2});")>]
let private removeEventListener (element:obj) (eventName:string) (handler:'T -> unit) : unit = failwith "never"

[<FunJS.JSEmit("return {0}.keyCode;")>]
let getKeyFromEvent e : byte = failwith "never"

/// A wrapper around DOM Events
type DomEvent<'T> (element, eventName) =   
   interface IEvent<'T> with
      member this.Subscribe (observer:IObserver<'T>) : IDisposable =
         let f = observer.OnNext
         addEventListener element eventName f
         let unsubscribe = fun() -> removeEventListener element eventName f
         upcast new ActionDisposable(unsubscribe)

      member this.AddHandler (f : 'T -> unit) = addEventListener element eventName f
      member this.RemoveHandler (f : 'T -> unit) = removeEventListener element eventName f

let main() =

    let txtCode = Dom.document.getElementById("code")    
    let submit = Dom.document.getElementById("execute_code")
    let console = Dom.document.getElementById("output")

    let consoleKeyPressEvt = new DomEvent<Dom.KeyboardEvent'>(console, "keypress") :> IEvent<_>

    let getNextChar =             
       Async.FromContinuations(fun(cont, error, cancelled) ->
          alert "GNC"  
          let sub = ref Unchecked.defaultof<IDisposable>
          let nextChar (e:Dom.KeyboardEvent') = 
              (!sub).Dispose()
              cont <| getKeyFromEvent e
          let observer = ActionObserver<Dom.KeyboardEvent'>(nextChar, (fun e -> ()), (fun () -> ()))
          sub := consoleKeyPressEvt.Subscribe(observer)) //TODO: Implement Observable.Take to avoid this mess
    
    let outputChar = console |> output
          
    let executeCode (e:Dom.MouseEvent') =        
        let code = txtCode |> getValue
        run code getNextChar outputChar |> Async.StartImmediate
        ignore |> box

    submit.onclick <- executeCode    

// Compile
let source = <@@ main() @@> |> Compiler.compileWithoutReturn 
let filename = "brainfuck.js"
System.IO.File.Delete filename
System.IO.File.WriteAllText(filename, source)
source|> printfn "%A"
System.Console.ReadLine() |> ignore