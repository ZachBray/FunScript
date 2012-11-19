module FunJS.Compiler

open FunJS

//TODO: Use IoC here. MiniIoC perhaps? 
let private allComponents =
   [  //NOTE: ORDER MATTERS!
      Options.components
      Seqs.components
      Sets.components
      Lists.components
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
   return this > that
      ? 1
      : this < that
         ? -1
         : 0;
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

let compile expr =
   let compiler = InternalCompiler.Compiler(allComponents)
   let program = compiler.Compile ReturnStrategies.returnFrom expr
   let reflectedDefs = compiler.Globals
   let block = List.append reflectedDefs program 
   comparerPrototypes + (AST.Block block).Print()

