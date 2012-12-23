(**
# FunScript Tutorial

This example shows basic features of FunScript. It demonstrates how to use the TypeScript
type provider to access standard DOM library functions and to call JavaScript libraries 
(such as JQuery). 

## Project structure

FunScript is a library that can be called in a number of ways. Most of the samples in the
`Examples` directory follow the same pattern - they are executable programs that, when
started, generate JavaScript source code (as `*.js` file) and run a simple HTTP web 
server that can be used to view the generated web.

In this mode, a typical project needs to reference `FunScript.dll` (containing the core
JavaScript translator), `FunScript.TypeScript.dll` (a type provider that generates nice
types for calling JavaScript libraries) and `FunScript.TypeScript.Interop.dll` (which 
is required by the TypeScript type provider).

All code that is translated to JavaScript needs to be marked with the `FunScript.JS`
attribute (an alias for `ReflectedDefinition`):
*)
[<FunScript.JS>]
module Page

open FunScript
open FunScript.TypeScript

(**
The `FunScript.TypeScript` namespace contains the TypeScript provider, which is 
discussed in the next section.

## Using the TypeScript provider

[TypeScript](http://www.typescriptlang.org/) is a language that extends JavaScript 
with types. It uses _definition files_ (`*.d.ts`) to specify the types of existing 
JavaScript libraries. FunScript can import information from TypeScript definition files 
and generate F# types for accessing the DOM and calling external JavaScript libraries.

The following two lines import definitions for [jQuery](https://github.com/ZachBray/FunScript/blob/master/Examples/Typings/jquery.d.ts)
and [JavaScript DOM](https://github.com/ZachBray/FunScript/blob/master/Examples/Typings/lib.d.ts) 
from TypeScript:
*)

type j = Api<"../Typings/jquery.d.ts">
type lib = Api<"../Typings/lib.d.ts">

(**
Hello world
*)

let hello () = 
  lib.window.alert("Hello world!")

(**
## Demo using mini F# async
*)

let (?) (jq:j.JQueryStatic) name = jq.Invoke("#" + name)

let main() = 
  j.jQuery?helloWorld.click(fun _ -> hello())

do Runtime.Run(components=Interop.Components.all)