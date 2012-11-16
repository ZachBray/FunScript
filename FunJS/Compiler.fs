module FunJS.Compiler

open FunJS

//TODO: Use IoC here. MiniIoC perhaps? 
let private allComponents =
   [  //NOTE: ORDER MATTERS!
      Options.components
      Seqs.components
      Lists.components
      ReflectedDefinitions.components
      Arrays.components
      RecordTypes.components
      UnionTypes.components
      Tuples.components
      Objects.components
      Logic.components
      LambdaApplication.components
      LetBindings.components
      Arithmetic.components
      Comparison.components
      PrimitiveTypes.components
      ControlFlow.components
      CommonOperators.components
   ] |> List.concat



let compile expr =
   let compiler = InternalCompiler.Compiler(allComponents)
   let program = compiler.Compile ReturnStrategies.returnFrom expr
   let reflectedDefs = compiler |> ReflectedDefinitions.preCompile expr
   let block = List.append reflectedDefs program 
   (AST.Block block).Print()

