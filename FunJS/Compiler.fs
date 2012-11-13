module FunJS.Compiler

open FunJS

//TODO: Use IoC here. MiniIoC perhaps? 
let private allComponents =
   [  //NOTE: ORDER MATTERS!
      CommonOperators.components
      Options.components
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
   ] |> List.concat



let compile expr =
   let compiler = InternalCompiler.Compiler(allComponents)
   let reflectedDefs = compiler |> ReflectedDefinitions.preCompile expr
   let program = compiler.Compile ReturnStrategies.returnFrom expr
   let block = List.append reflectedDefs program 
   (AST.Block block).Print(0, ref Map.empty)

