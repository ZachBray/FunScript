module (*internal*) FunScript.CompilerComponent

open AST
open InternalCompiler
open Microsoft.FSharp.Quotations

/// This is to cope with nested stuff that is supported in F#
/// but not JS. For example, f(if x then y else z)
let private splitDeclarationFromUsageIfNeeded (compiler:ICompiler) expr =
   let firstAttempt = compiler.Compile ReturnStrategies.inplace expr
   match firstAttempt with
   | Do expr::[] -> [], expr
   | DeclareAndAssign(var, valExpr)::Do(Reference(var2) as refExpr)::[] when var = var2 ->
      [], valExpr
   | multipleStatements ->
      let var = compiler.NextTempVar()
      let declsAndRef =
         [  yield Declare [var]
            yield! compiler.Compile (ReturnStrategies.assignVar var) expr
         ], Reference var
      declsAndRef

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
   match replacement with
   | None | Some(DerivedPatterns.MethodWithReflectedDefinition _) -> ()
   | Some _ -> failwith "Replacement had no reflected definition"
   { Target = methodInfo
     TargetType = callType
     Replacement = replacement
     TryReplace = makeFriendly replace
   } |> CallReplacer

open Microsoft.FSharp.Quotations
open System.Reflection

let generateArityWithCompiler quote (|ArgMatch|_|) =
   let mi, callType = Quote.toMethodBaseFromLambdas quote
   createCallerReplacer mi callType None <| fun split compiler returnStategy ->
      function
      | None, typeArgs, expr ->
         match expr with
         | ArgMatch mi typeArgs split compiler (decls, code) ->
            [ yield! decls |> List.concat
              yield returnStategy.Return code
            ]
         | _ -> []
      | Some obj, typeArgs, args ->
         match obj::args with
         | ArgMatch mi typeArgs split compiler (decls, code) ->
            [ yield! decls |> List.concat
              yield returnStategy.Return code
            ]
         | _ -> []

let generateArity quote argMatch = 
   generateArityWithCompiler quote (fun mi _ x _ -> argMatch mi x)

let nullary quote code =
   generateArity quote (fun _ (|Split|) ->
      function
      | [] -> Some([], code)
      | _ -> None)

let unary quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(decl, ref)] -> Some([decl], genCode ref)
      | _ -> None)

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
         | Some code -> Some ([declLHS; declRHS], code)   
         | None -> None      
      | _ -> None)

let binary quote genCode = binaryTyped quote (fun _ _ a _ b -> Some(genCode a b))

let binaryStatement quote genCode =
   generateArity quote (fun _ (|Split|) ->
      function
      | [Split(declLHS, refLHS); Split(declRHS, refRHS)] ->
         Some ([declLHS; declRHS; [genCode refLHS refRHS]], Null)         
      | _ -> None)

let binaryOp quote symbol =
   binary quote (fun refLHS refRHS -> BinaryOp(refLHS, symbol, refRHS))

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