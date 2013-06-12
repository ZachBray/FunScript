module FunScript.Compiler

open FunScript

//TODO: Use IoC here. MiniIoC perhaps? 
let private allComponents =
   [  //NOTE: ORDER MATTERS!
      Options.components
      Seqs.components
      Sets.components
      Maps.components
      Lists.components
      Strings.components
      Asyncs.components
      ReflectedDefinitions.components
      Arrays.components
      RecordTypes.components
      UnionTypes.components
      Tuples.components
      Logic.components
      LambdaApplication.components
      LetBindings.components
      Arithmetic.components
      Comparison.components
      PrimitiveTypes.components
      ControlFlow.components
      CommonOperators.components
      Objects.components
      Reflection.components
   ] |> List.concat

type Compiler =
   static member Compile(expression, ?components, ?noReturn, ?shouldFlattenGenericsForReflection) = 
      let components = defaultArg components []
      let shouldFlattenGenericsForReflection = defaultArg shouldFlattenGenericsForReflection true
      let returnStrat = 
         if defaultArg noReturn false then ReturnStrategies.inplace
         else ReturnStrategies.returnFrom
      let compiler = InternalCompiler.Compiler(allComponents @ components, shouldFlattenGenericsForReflection)
      let program = compiler.Compile returnStrat expression
      let reflectedDefs = compiler.Globals
      let block = List.append reflectedDefs program 
      (AST.Block block).Print()
      
let compile expr = Compiler.Compile(expr)
let compileWithoutReturn expr = Compiler.Compile(expr, noReturn=true)
