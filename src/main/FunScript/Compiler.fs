module FunScript.Compiler

open FunScript

//TODO: Use IoC here. MiniIoC perhaps? 
let createComponents(isEventMappingEnabled) =
    [  //NOTE: ORDER MATTERS!
        yield! Options.components
        yield! Seqs.components
        yield! Sets.components
        yield! Maps.components
        yield! Lists.components
        yield! Strings.components
        yield! Regexs.components
        yield! Times.components
        yield! Asyncs.components
        yield! ReflectedDefinitions.components
        yield! Arrays.components
        yield! ResizeArrays.components
        yield! Dictionaries.components
        yield! RecordTypes.components
        yield! UnionTypes.components
        yield! Tuples.components
        yield! Logic.components
        yield! LambdaApplication.components
        yield! LetBindings.components
        yield! Arithmetic.components
        yield! Comparison.components
        yield! PrimitiveTypes.components
        yield! ControlFlow.components
        yield! Objects.components
        yield! Reflection.components
        yield! TypeConversions.components
        yield! OtherMappings.components
        if isEventMappingEnabled then
            yield! OtherMappings.eventComponents
        yield! CommonOperators.components
    ]

type Compiler =
   static member private CompileImpl(expression, adjustComponents, noReturn, shouldCompress, isEventMappingEnabled) = 
      let shouldCompress = defaultArg shouldCompress false
      let isEventMappingEnabled = defaultArg isEventMappingEnabled true
      let returnStrat = 
         if defaultArg noReturn false then ReturnStrategies.inplace
         else ReturnStrategies.returnFrom
      let components = createComponents isEventMappingEnabled
      let compiler = InternalCompiler.Compiler(adjustComponents components)
      let program = compiler.Compile returnStrat expression
      let reflectedDefs = compiler.Globals
      let block = List.append reflectedDefs program
      if shouldCompress then (AST.Block block).PrintCompressed()
      else (AST.Block block).Print()

   static member Compile(expression, adjustComponents, ?noReturn, ?shouldCompress, ?isEventMappingEnabled) = 
      Compiler.CompileImpl(expression, adjustComponents, noReturn, shouldCompress, isEventMappingEnabled)

   static member Compile(expression, ?components, ?noReturn, ?shouldCompress, ?isEventMappingEnabled) = 
      let components = defaultArg components []
      Compiler.CompileImpl(expression, (fun existingComponents -> existingComponents @ components), noReturn, shouldCompress, isEventMappingEnabled)
      
let compile expr = Compiler.Compile(expr)
let compileWithoutReturn expr = Compiler.Compile(expr, noReturn=true)
