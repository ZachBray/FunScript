module internal FunScript.Tuples

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let itemsPropName = "Items"
let getItem i ref = IndexGet(PropertyGet(ref, itemsPropName), Number(float i))

let genComparisonFunc ts =
   let that = Var("that", typeof<obj>)
   let diff = Var("diff", typeof<obj>)
      
   let fields =
      ts |> List.mapi (fun i x -> i, x)

   let body =
      List.foldBack (fun (i, t) acc ->
         let thisField = This |> getItem i
         let thatField = Reference that |> getItem i
         let decls, compareExpr = Comparison.compareCall t thisField t thatField |> Option.get
         [  yield! decls
            yield Assign(Reference diff, compareExpr)
            yield IfThenElse(
               BinaryOp(Reference diff, "!=", Number 0.),
               Block [ Return <| Reference diff ],
               Block acc)
         ]) fields [ Return <| Number 0. ]
      
   Lambda(
      [that],
      Block <| DeclareAndAssign(diff, Number 0.) :: body
   )

let genComparisonMethods ts =
    let func = genComparisonFunc ts
    [ "CompareTo", func ]

let getTupleVars n =
   [ 0 .. n - 1] |> List.map (fun i -> Var(sprintf "Item%i" i, typeof<obj>))

let private createConstructor n compiler =
   let vars = getTupleVars n
   let refs = vars |> List.map Reference
   vars, Block [
      for var in vars do 
          yield Assign(PropertyGet(This, itemsPropName), Array refs)
   ]

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.NewTuple(exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let n = exprs.Length
         let typeArgs = exprs |> List.map (fun expr -> expr.Type)
         let specialization = Reflection.getSpecializationString compiler typeArgs
         let name = sprintf "Tuple%s" specialization
         let cons = 
            compiler.DefineGlobal name (fun var -> 
               [  yield Assign(Reference var, Lambda <| createConstructor n compiler) 
                  let methods = genComparisonMethods typeArgs
                  let proto = PropertyGet(Reference var, "prototype")
                  for name, lambda in methods do
                     yield Assign(PropertyGet(proto, name), lambda)
               ])
         [  yield! decls |> Seq.concat 
            yield returnStategy.Return <| New(cons.Name, refs)
         ]
      | _ -> []

let private getIndex =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.TupleGet(Split(valDecl, valRef), i) ->
         [ yield! valDecl
           yield returnStategy.Return (valRef |> getItem i)
         ]
      | _ -> []

let components = [ 
   creation
   getIndex
   CompilerComponent.unary <@ fst @> (fun arg -> arg |> getItem 0)
   CompilerComponent.unary <@ snd @> (fun arg -> arg |> getItem 1)
   CompilerComponent.unary 
      <@ Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields @> 
      (fun arg -> PropertyGet(arg, itemsPropName))
   CompilerComponent.binary 
      <@ fun obj i -> Microsoft.FSharp.Reflection.FSharpValue.GetTupleField(obj, i) @> 
      (fun arg i -> IndexGet(PropertyGet(arg, itemsPropName), i))
]