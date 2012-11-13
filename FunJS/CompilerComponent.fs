module internal FunJS.CompilerComponent

open AST
open InternalCompiler

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


let createGetterReplacer propInfo replacement replace =
   { PropertyGetReplacer.Target = propInfo
     PropertyGetReplacer.Replacement = replacement
     PropertyGetReplacer.TryReplace = makeFriendly replace
   } |> PropertyGetReplacer

let createSetterReplacer propInfo replacement replace =
   { PropertySetReplacer.Target = propInfo
     PropertySetReplacer.Replacement = replacement
     PropertySetReplacer.TryReplace = makeFriendly replace
   } |> PropertySetReplacer

let createCallerReplacer methodInfo replacement replace =
   { Target = methodInfo
     Replacement = replacement
     TryReplace = makeFriendly replace
   } |> MethodCallReplacer


open Microsoft.FSharp.Quotations
open System.Reflection

let mostGeneric(mi:MethodInfo)  = 
   if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
   else mi

module Quote =
   let toPropertyInfo = function
      | Patterns.PropertyGet(_,pi,_)
      | Patterns.PropertySet(_,pi,_,_)
      | DerivedPatterns.Lambdas(_,Patterns.PropertyGet(_,pi,_))
      | DerivedPatterns.Lambdas(_,Patterns.PropertySet(_,pi,_,_)) -> pi
      | _ -> failwith "Expected a property call wrapped in a lambda"

   let toMethodInfo = function
      | Patterns.Call(_,mi,_)
      | DerivedPatterns.Lambdas(_,Patterns.Call(_,mi,_)) -> mostGeneric mi
      | _ -> failwith "Expected a method call wrapped in a lambda"
   

let private generateArity quote (|ArgMatch|_|) =
   let mi = Quote.toMethodInfo quote
   createCallerReplacer mi None <| fun split (|Return|) returnStategy ->
      function
      | None, _, ArgMatch split (decls, code) ->
         [ yield! decls |> List.concat
           yield returnStategy.Return code
         ]
      | Some obj, _, args ->
         match obj::args with
         | ArgMatch split (decls, code) ->
            [ yield! decls |> List.concat
              yield returnStategy.Return code
            ]
         | _ -> []
      | _ -> []

let nullary quote code =
   generateArity quote (fun (|Split|) ->
      function
      | [] -> Some([], code)
      | _ -> None)

let unary quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(decl, ref)] -> Some([decl], genCode ref)
      | _ -> None)

let unaryStatement quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(decl, ref)] -> Some([decl; [genCode ref]], Null)
      | _ -> None)

let unaryOp quote symbol =
   unary quote (fun refArg -> UnaryOp(symbol, refArg))

let binary quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(declLHS, refLHS); Split(declRHS, refRHS)] ->
         Some ([declLHS; declRHS], genCode refLHS refRHS)         
      | _ -> None)

let binaryStatement quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(declLHS, refLHS); Split(declRHS, refRHS)] ->
         Some ([declLHS; declRHS; [genCode refLHS refRHS]], Null)         
      | _ -> None)

let binaryOp quote symbol =
   binary quote (fun refLHS refRHS -> BinaryOp(refLHS, symbol, refRHS))

let ternary quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(declA, refA); Split(declB, refB); Split(declC, refC)] ->
         Some ([declA; declB; declC], genCode refA refB refC)         
      | _ -> None)

let ternaryStatement quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(declA, refA); Split(declB, refB); Split(declC, refC)] ->
         Some ([declA; declB; declC; [genCode refA refB refC]], Null)         
      | _ -> None)

let nary4 quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(declA, refA); 
         Split(declB, refB); 
         Split(declC, refC);
         Split(declD, refD)] ->
         Some ([declA; declB; declC; declD], genCode refA refB refC refD)         
      | _ -> None)

let nary5 quote genCode =
   generateArity quote (fun (|Split|) ->
      function
      | [Split(declA, refA); 
         Split(declB, refB); 
         Split(declC, refC);
         Split(declD, refD);
         Split(declE, refE)] ->
         Some ([declA; declB; declC; declD; declE], genCode refA refB refC refD refE)         
      | _ -> None)