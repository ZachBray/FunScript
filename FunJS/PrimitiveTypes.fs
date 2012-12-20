module internal FunJS.PrimitiveTypes

open AST
open Microsoft.FSharp.Quotations

let private primitiveValues =
   CompilerComponent.create <| fun _ _ returnStategy ->
      function
      | DerivedPatterns.Unit x -> [ Empty ]
      | DerivedPatterns.Bool x -> [ yield returnStategy.Return <| Boolean x ]
      | DerivedPatterns.Char x -> [ yield returnStategy.Return <| String (string x) ]
      | DerivedPatterns.String x -> [ yield returnStategy.Return <| String x ]
      | DerivedPatterns.SByte x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Byte x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Int16 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Int32 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Int64 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt16 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt32 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt64 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Single x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Double x -> [ yield returnStategy.Return <| Number(x) ]
      | Patterns.Value(null, _) -> [ yield returnStategy.Return <| Null ]
      | _ -> []

let components = [ 
   primitiveValues
]