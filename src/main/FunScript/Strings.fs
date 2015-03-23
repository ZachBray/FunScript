module internal FunScript.Strings

open AST
open Microsoft.FSharp.Quotations

[<FunScript.JSEmitInline("''")>]
let private getEmptyString(): string = failwith "never"

let private extraComponents =
    let compileNonGenericQuote (compiler: InternalCompiler.ICompiler) returnStrategy quote args =
        let mi, _ = Quote.toMethodInfoFromLambdas quote
        compiler.Compile returnStrategy (ExpressionReplacer.buildCall mi args)
    CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
        function
        | Patterns.FieldGet(_, fi) when fi.DeclaringType = typeof<string> && fi.Name = "Empty" ->
            compileNonGenericQuote compiler returnStrategy <@ getEmptyString @> []
        | Patterns.Call(_, mi, [Patterns.Coerce(Patterns.NewObject(_,args),_)]) when mi.Name = "PrintFormatToString" ->
            compileNonGenericQuote compiler returnStrategy <@ Core.String.PrintFormatToString @> args
        | _ -> []

let components = 
   [
      [
         extraComponents
         ExpressionReplacer.createUnsafe <@ fun (s:string) (chars:char[]) -> s.Split(chars) @> <@ Core.String.SplitWithoutOptions @>
         ExpressionReplacer.createUnsafe 
            <@ fun (s:string) (chars:char[]) (opts:System.StringSplitOptions) -> s.Split(chars,opts) @> 
            <@ Core.String.SplitWithOptions @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Length @> <@ Core.String.Length @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.ToCharArray() @> <@ Core.String.ToCharArray @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Replace("", "") @> <@ Core.String.Replace @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.IndexOf : string -> int @> <@ Core.String.IndexOfWithoutOffset @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.IndexOf : string * int -> int @> <@ Core.String.IndexOfWithOffset @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.LastIndexOf : string -> int @> <@ Core.String.LastIndexOfWithoutOffset @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.LastIndexOf : string * int -> int @> <@ Core.String.LastIndexOfWithOffset @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.StartsWith : string -> bool @> <@ Core.String.StartsWith @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.EndsWith : string -> bool @> <@ Core.String.EndsWith @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Trim() @> <@ Core.String.Trim @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.ToLower() @> <@ Core.String.ToLowerCase @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.ToUpper() @> <@ Core.String.ToUpperCase @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Length @> <@ Core.String.Length @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.[0] @> <@ Core.String.CharAt @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Substring : int -> string @> <@ Core.String.SubstringWithoutLength @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Substring : int * int -> string @> <@ Core.String.SubstringWithLength @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Contains : string -> bool @> <@ Core.String.Contains @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Core.StringModule"
         "FunScript" "FunScript.Core.String.FSharpString"
      ExpressionReplacer.createModuleMapping 
         "mscorlib" "System.String"
         "FunScript" "FunScript.Core.String"
   ] |> List.concat