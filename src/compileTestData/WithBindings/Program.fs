[<ReflectedDefinition>]
module Page

open FunScript
open FunScript.TypeScript

let hello () =
    Globals.window.alert("Hello world!")
     
let jq(selector : string) = Globals.Dollar.Invoke selector
let (?) jq name = jq("#" + name)

[<FunScript.AssemblyMain>]
let main() = 
    jq?helloWorld.click(fun _ -> hello() :> obj)