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
   ] |> List.concat

let private comparerPrototypes = """

Boolean.prototype.CompareTo = function(that) {
   return this - that;
};

Number.prototype.CompareTo = function(that) {
   return this - that;
};

String.prototype.CompareTo = function(that) {
   if (this > that) {
      return 1;
   } else if (this < that) {
      return -1;
   }
   else {
      return 0;
   }
};

Array.prototype.CompareTo = function(that) {
   var i = 0;
   while(i < this.length && i < that.length) {
      var diff = this[i].CompareTo(that[i]);
      if(diff != 0) {
         return diff;
      }
      i = i + 1;
   };
   return this.length - that.length;
};

"""

type Compiler =
   static member Compile(expression, ?components, ?noReturn) = 
      let components = defaultArg components []
      let returnStrat = 
         if defaultArg noReturn false then ReturnStrategies.inplace
         else ReturnStrategies.returnFrom
      let compiler = InternalCompiler.Compiler(allComponents @ components)
      let program = compiler.Compile returnStrat expression
      let reflectedDefs = compiler.Globals
      let block = List.append reflectedDefs program 
      comparerPrototypes + (AST.Block block).Print()
      
let compile expr = Compiler.Compile(expr)
let compileWithoutReturn expr = Compiler.Compile(expr, noReturn=true)
