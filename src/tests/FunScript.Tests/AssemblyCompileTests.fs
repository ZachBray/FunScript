#if INTERACTIVE
#load "Interactive.fsx"
open FunScript.Tests.Common
#endif
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.CompileTests

open System
open System.Reflection
open NUnit.Framework


[<Test>]
let ``Project compiles to js``() =
    let compiled = FunScript.Compiler.Compiler.CompileAssembly(Assembly.Load("Simple"), false, false, true)
    let expected = 
        """var Page__main$;
  Page__main$ = (function(unitVar0)
  {
    var foo = "bar";
    return foo;
  });
  return Page__main$()"""
    Assert.AreEqual(expected, compiled)