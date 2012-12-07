[<FunJS.JS>]
module FunJS.Core.String

[<FunJS.JSEmit("return {0}.split({1});")>]
let private splitSingle(s:string, delimiter:string) : string[] = failwith "never"

[<FunJS.JSEmit("return {0}.replace({1}, {2});")>]
let private replaceSingle(s:string, search:string, replace:string) : string = failwith "never"

[<FunJS.JSEmit("return {0}.indexOf({1});")>]
let IndexOf(s:string, search:string) : int = failwith "never"

[<FunJS.JSEmit("return {0}.toLowerCase();")>]
let ToLowerCase(s:string) : string = failwith "never"

[<FunJS.JSEmit("return {0}.toUpperCase();")>]
let ToUpperCase(s:string) : string = failwith "never"

[<FunJS.JSEmit("return ({0}==null)||({0}==\"\");")>]
let IsNullOrEmpty(s:string) : string = failwith "never"

[<FunJS.JSEmit("return {0}.length;")>]
let Length(s:string) : int = failwith "never"

[<FunJS.JSEmit("return {0}.charAt({1});")>]
let CharAt(s:string, length:int) : char = failwith "never"

[<FunJS.JSEmit("return String.fromCharCode({0});")>]
let inline FromCharCode x = char x

let Split(s:string, delimiters:string[]) : string[] = 
   delimiters |> Array.fold (fun inputs delimiter ->
      inputs |> Array.map (fun inp -> splitSingle(inp, delimiter))
             |> Array.concat) [| s |]

let Replace(s:string, search:string, replace:string) : string = 
   let mutable res = s
   while res.IndexOf(search) > -1 do
      res <- replaceSingle(res, search, replace)
   res 
