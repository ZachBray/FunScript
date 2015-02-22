module internal FunScript.PrimitiveTypes

open AST
open Microsoft.FSharp.Quotations

let private primitiveValues =
   CompilerComponent.create <| fun _ _ returnStrategy ->
      function
      | DerivedPatterns.Unit x -> [ Empty ]
      | DerivedPatterns.Bool x -> [ yield returnStrategy.Return <| Boolean x ]
      | DerivedPatterns.Char x -> [ yield returnStrategy.Return <| String (string x) ]
      | DerivedPatterns.String x -> [ yield returnStrategy.Return <| String x ]
      | DerivedPatterns.SByte x -> [ yield returnStrategy.Return <| Integer(int x) ]
      | DerivedPatterns.Byte x -> [ yield returnStrategy.Return <| Integer(int x) ]
      | DerivedPatterns.Int16 x -> [ yield returnStrategy.Return <| Integer(int x) ]
      | DerivedPatterns.Int32 x -> [ yield returnStrategy.Return <| Integer(x) ]
      | DerivedPatterns.Int64 x -> [ yield returnStrategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt16 x -> [ yield returnStrategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt32 x -> [ yield returnStrategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt64 x -> [ yield returnStrategy.Return <| Number(float x) ]
      | DerivedPatterns.Single x -> [ yield returnStrategy.Return <| Number(float x) ]
      | DerivedPatterns.Double x -> [ yield returnStrategy.Return <| Number(x) ]
      // TODO: our own decimal type?
      | Patterns.Value(null, _) -> [ yield returnStrategy.Return <| Null ]
      | Patterns.Value(x, t) ->
            if t.IsEnum then 
                // TODO: Remove this hack. Replace with Attribute.
                if t.Assembly.GetName().Name.StartsWith("FunScript.TypeScript.Binding.") then
                    // TODO: Add attribute for this too. It could be sanitized by this point!
                    let name = System.Enum.GetName(t, x)
                    [ yield returnStrategy.Return <| JSExpr.EmitExpr(fun (_, _) -> t.FullName + "." + name) ] 
                else [ yield returnStrategy.Return <| Integer(unbox x) ]
            else []
      | _ -> []

let components = [ 
   primitiveValues
]