module internal FunJS.UnionTypes

open AST
open System.Reflection
open Microsoft.FSharp.Quotations

let private getPropNames(caseType:System.Type) = 
   caseType.GetProperties()
   |> Seq.map (fun p -> p.Name, p.GetCustomAttribute<CompilationMappingAttribute>())
   |> Seq.filter (fun (name, attr) -> not <| obj.ReferenceEquals(null, attr)) 
   |> Seq.filter (fun (name, attr) -> SourceConstructFlags.Field = attr.SourceConstructFlags)
   |> Seq.sortBy (fun (name, attr) -> attr.SequenceNumber)
   |> Seq.map fst
   |> Seq.toList

let private creation =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.NewUnionCase(uci, exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let unionType = uci.DeclaringType
         let propNames = 
            match unionType.GetMember(uci.Name).[0] with
            | :? System.Type as caseType -> getPropNames caseType
            | :? MethodInfo -> getPropNames uci.DeclaringType
            | :? PropertyInfo -> []
            | _ -> failwith "never"         
         let fields = 
            ("Tag", String uci.Name) ::
            (List.zip propNames refs)
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| Object fields
         ]
      | _ -> []

let private matching =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.UnionCaseTest(Split(objDecl, objRef), uci) ->
         [ yield! objDecl
           yield returnStategy.Return <| BinaryOp(PropertyGet(objRef, "Tag"), "==", String(uci.Name))
         ]
      | _ -> []

let components = [ 
   creation
   matching
]