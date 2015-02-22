module (*internal*) FunScript.CompilerComponent

open AST
open InternalCompiler
open Microsoft.FSharp.Quotations

/// This is to cope with nested stuff that is supported in F#
/// but not JS. For example, f(if x then y else z)
let private splitDeclarationFromUsageIfNeeded (compiler:ICompiler) expr =
    let var = compiler.NextTempVar()
    let statements = compiler.Compile (ReturnStrategies.assignVar var) expr 
    let statementsArray = statements |> List.toArray
    match statementsArray.[statementsArray.Length - 1] with
    | Assign(Reference v, expr) when v = var ->
        statementsArray.[0..statementsArray.Length - 2] |> Array.toList, 
        expr
    | _ -> 
        [yield Declare[var]
         yield! statements], Reference var

let makeFriendly transform compiler =
   let split = splitDeclarationFromUsageIfNeeded compiler
   transform split compiler

let create compile =
   {  new ICompilerComponent with
         member __.TryCompile compiler returnStrategy expr =
            let compile = makeFriendly compile compiler
            compile returnStrategy expr
            |> Seq.toList
   } |> CompilerComponent

let createCallerReplacer methodInfo callType replacement replace =
   { Target = methodInfo
     TargetType = callType
     Replacement = replacement
     TryReplace = makeFriendly replace
   } |> CallReplacer

open Microsoft.FSharp.Quotations
open System.Reflection

let generateMacro quote genExpr =
    let mi, callType = Quote.toMethodBaseFromLambdas quote
    createCallerReplacer mi callType None <| fun split compiler returnStrategy args ->
        let exprs =
            match args with
            | None, _, exprs -> exprs
            | Some expr, _, exprs -> expr::exprs
        match genExpr split compiler returnStrategy exprs with
        | None -> []
        | Some fixedExpr -> compiler.Compile returnStrategy fixedExpr

let generateBinaryMacro quote genExpr =
    generateMacro quote (fun split compiler returnStrategy ->
        function
        | [exprA; exprB] -> genExpr split compiler returnStrategy exprA exprB
        | _ -> None)

let generateArityWithCompiler quote (|ArgMatch|_|) =
   let mi, callType = Quote.toMethodBaseFromLambdas quote
   createCallerReplacer mi callType None <| fun split compiler returnStrategy ->
      function
      | None, typeArgs, expr ->
         match expr with
         | ArgMatch mi typeArgs split compiler (decls, code) ->
            [ yield! decls |> List.concat
              yield returnStrategy.Return code
            ]
         | _ -> []
      | Some obj, typeArgs, args ->
         match obj::args with
         | ArgMatch mi typeArgs split compiler (decls, code) ->
            [ yield! decls |> List.concat
              yield returnStrategy.Return code
            ]
         | _ -> []

let generateArity quote argMatch = 
   generateArityWithCompiler quote (fun mi _ x _ -> argMatch mi x)

let nullary quote code =
   generateArity quote (fun _ (|Split|) ->
      function
      | [] -> Some([], code)
      | _ -> None)

let unaryTyped quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(decl, ref) as expr] -> 
         genCode expr.Type ref |> Option.map (fun x -> [decl], x)
      | _ -> None)

let unary quote genCode = unaryTyped quote (fun _ x -> Some(genCode x))

let unaryStatement quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(decl, ref)] -> Some([decl; [genCode ref]], Null)
      | _ -> None)

let unaryOp quote symbol =
   unary quote (fun refArg -> UnaryOp(symbol, refArg))

let binaryTyped quote genCode =
   generateArity quote (fun mi (|Split|) ->
      function
      | [Split(declLHS, refLHS) as exprA; Split(declRHS, refRHS) as exprB] ->
         match genCode mi exprA.Type refLHS exprB.Type refRHS with
         | Some (decls, code) -> Some ([declLHS; declRHS; decls], code)   
         | None -> None      
      | _ -> None)

let binaryTypedExpr quote genCode =
    generateArity quote (fun _ split ->
      function
      | [exprA; exprB] -> genCode split exprA exprB  
      | _ -> None)

let binary quote genCode = binaryTyped quote (fun _ _ a _ b -> Some([], genCode a b))

let binaryStatement quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(declLHS, refLHS); Split(declRHS, refRHS)] ->
         Some ([declLHS; declRHS; [genCode refLHS refRHS]], Null)         
      | _ -> None)

let ternary quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(declA, refA); Split(declB, refB); Split(declC, refC)] ->
         Some ([declA; declB; declC], genCode refA refB refC)         
      | _ -> None)

let ternaryStatement quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(declA, refA); Split(declB, refB); Split(declC, refC)] ->
         Some ([declA; declB; declC; [genCode refA refB refC]], Null)         
      | _ -> None)

let nary4 quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(declA, refA); 
         Split(declB, refB); 
         Split(declC, refC);
         Split(declD, refD)] ->
         Some ([declA; declB; declC; declD], genCode refA refB refC refD)         
      | _ -> None)

let nary5 quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(declA, refA); 
         Split(declB, refB); 
         Split(declC, refC);
         Split(declD, refD);
         Split(declE, refE)] ->
         Some ([declA; declB; declC; declD; declE], genCode refA refB refC refD refE)         
      | _ -> None)