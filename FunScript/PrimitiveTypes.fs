module internal FunScript.PrimitiveTypes

open AST
open Microsoft.FSharp.Quotations

let private primitiveValues =
   CompilerComponent.create <| fun _ _ returnStategy ->
      function
      | DerivedPatterns.Unit x -> [ Empty ]
      | DerivedPatterns.Bool x -> [ yield returnStategy.Return <| Boolean x ]
      | DerivedPatterns.Char x -> [ yield returnStategy.Return <| String (string x) ]
      | DerivedPatterns.String x -> [ yield returnStategy.Return <| String x ]
      | DerivedPatterns.SByte x -> [ yield returnStategy.Return <| Integer(int x) ]
      | DerivedPatterns.Byte x -> [ yield returnStategy.Return <| Integer(int x) ]
      | DerivedPatterns.Int16 x -> [ yield returnStategy.Return <| Integer(int x) ]
      | DerivedPatterns.Int32 x -> [ yield returnStategy.Return <| Integer(x) ]
      | DerivedPatterns.Int64 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt16 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt32 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.UInt64 x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Single x -> [ yield returnStategy.Return <| Number(float x) ]
      | DerivedPatterns.Double x -> [ yield returnStategy.Return <| Number(x) ]
      // TODO: our own decimal type?
      | Patterns.Value(null, _) -> [ yield returnStategy.Return <| Null ]
      | Patterns.Value(x, t) ->
            if t.IsEnum then 
                // TODO: Remove this hack. Replace with Attribute.
                if t.Assembly.GetName().Name.StartsWith("FunScript.TypeScript.Binding.") then
                    // TODO: Add attribute for this too. It could be sanitized by this point!
                    let name = System.Enum.GetName(t, x)
                    [ yield returnStategy.Return <| JSExpr.EmitExpr(fun (_, _) -> t.FullName + "." + name) ] 
                else [ yield returnStategy.Return <| Integer(unbox x) ]
            else []
      | _ -> []

let components = [ 
   primitiveValues
]