[<FunScript.JS>]
module FunScript.Core.String

[<FunScript.JSEmit("return {0}.split({1});")>]
let private splitSingle(s:string, delimiter:string) : string[] = failwith "never"

[<FunScript.JSEmit("return {0}.replace({1}, {2});")>]
let private replaceSingle(s:string, search:string, replace:string) : string = failwith "never"

[<FunScript.JSEmit("return {0}.indexOf({1});")>]
let IndexOf(s:string, search:string) : int = failwith "never"

[<FunScript.JSEmit("return {0}.toLowerCase();")>]
let ToLowerCase(s:string) : string = failwith "never"

[<FunScript.JSEmit("return {0}.toUpperCase();")>]
let ToUpperCase(s:string) : string = failwith "never"

[<FunScript.JSEmit("return ({0}==null)||({0}==\"\");")>]
let IsNullOrEmpty(s:string) : string = failwith "never"

[<FunScript.JSEmit("return {0}.length;")>]
let Length(s:string) : int = failwith "never"

[<FunScript.JSEmit("return {0}.charAt({1});")>]
let CharAt(s:string, length:int) : char = failwith "never"

[<FunScript.JSEmit("return String.fromCharCode({0});")>]
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

[<FunScript.JSEmit("return {1}.join({0});")>]
let Join(separator:string, s:string[]) : string = failwith "never"

[<FunScript.JSEmit("return {0};")>]
let ToCharArray(str:string) : char[] = failwith "never"

// Re-implementation of functions from Microsoft.FSharp.Core.StringModule
module FSharpString = 
   [<FunScript.JSEmit("return {0}==null?\"\":{0};")>]
   let private emptyIfNull (str:string) : string = failwith "never"

   let Concat sep (strings : seq<string>) =  
      Join(sep, Array.ofSeq strings)

   let Iterate (f : (char -> unit)) (str:string) =
      let str = emptyIfNull str
      for i = 0 to str.Length - 1 do
          f str.[i] 

   let IterateIndexed f (str:string) =
      let str = emptyIfNull str
      for i = 0 to str.Length - 1 do
          f i str.[i] 

   let Map (f: char -> char) (str:string) =
      let str = emptyIfNull str
      str.ToCharArray() |> Array.Map (fun c -> (f c).ToString()) |> Concat ""

   let MapIndexed (f: int -> char -> char) (str:string) =
      let str = emptyIfNull str
      str.ToCharArray() |> Array.MapIndexed (fun i c -> (f i c).ToString()) |> Concat ""

   let Collect (f: char -> string) (str:string) =
      let str = emptyIfNull str
      str.ToCharArray() |> Array.Map f |> Concat ""

   let Initialize (count:int) (initializer: int-> string) =
      if count < 0 then invalidArg "count" "String length must be non-negative"
      Array.Initialize count initializer |> Concat ""

   let Replicate (count:int) (str:string) =
      if count < 0 then  invalidArg "count" "String length must be non-negative"
      let str = emptyIfNull str
      Initialize count (fun _ -> str)

   let ForAll f (str:string) =
      let str = emptyIfNull str
      let rec check i = (i >= str.Length) || (f str.[i] && check (i+1)) 
      check 0

   let Exists f (str:string) =
      let str = emptyIfNull str
      let rec check i = (i < str.Length) && (f str.[i] || check (i+1)) 
      check 0  

   let Length (str:string) =
      let str = emptyIfNull str
      str.Length


