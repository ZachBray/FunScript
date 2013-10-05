module internal FunScript.Strings

open AST
open Microsoft.FSharp.Quotations

[<JS>]
module Redirects =
    let substring (str:string) i =
        str.Substring(i, str.Length - i)

let components = 
   [
      [
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Split @> <@ Core.String.Split @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Length @> <@ Core.String.Length @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.ToCharArray() @> <@ Core.String.ToCharArray @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Replace("", "") @> <@ Core.String.Replace @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.IndexOf("") @> <@ Core.String.IndexOf @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.ToLower() @> <@ Core.String.ToLowerCase @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.ToUpper() @> <@ Core.String.ToUpperCase @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.Length @> <@ Core.String.Length @>
         ExpressionReplacer.createUnsafe <@ fun (s:string) -> s.[0] @> <@ Core.String.CharAt @>
         ExpressionReplacer.create <@ fun (s:string) -> s.Substring @> <@ Redirects.substring @>
         // ExpressionReplacer.create <@ List.toSeq @> <@ Core.Seq.OfList @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Core.StringModule"
         "FunScript" "FunScript.Core.String.FSharpString"
      ExpressionReplacer.createModuleMapping 
         "mscorlib" "System.String"
         "FunScript" "FunScript.Core.String"
   ] |> List.concat