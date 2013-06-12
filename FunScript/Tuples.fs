module internal FunScript.Tuples

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let getPropNames n =
   match n with
   | 1 -> [ "Value" ]
   | n -> [ for i = 1 to n do  yield sprintf "Item%i" i ]

let getTupleVars n =
   getPropNames n |> List.map (fun name -> Var(name, typeof<obj>))

let genComparisonFunc ts =
   let that = Var("that", typeof<obj>)
   let diff = Var("diff", typeof<obj>)
      
   let fields =
      let propNames = ts |> List.length |> getPropNames
      List.zip propNames ts

   let body =
      List.foldBack (fun (name, t) acc ->
         let thisField = PropertyGet(This, name)
         let thatField = PropertyGet(Reference that, name)
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

let private createConstructor n compiler =
   let vars = getTupleVars n
   vars, Block [  
      for var in vars do yield Assign(PropertyGet(This, var.Name), Reference var)
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
           yield returnStategy.Return <| PropertyGet(valRef, sprintf "Item%i" (i+1))
         ]
      | _ -> []

let components = [ 
   creation
   getIndex
   CompilerComponent.unary <@ fst @> (fun arg -> PropertyGet(arg, "Item1"))
   CompilerComponent.unary <@ snd @> (fun arg -> PropertyGet(arg, "Item2"))
]