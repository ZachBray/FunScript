[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.ModuleTests

open System
open NUnit.Framework

[<ReflectedDefinition>]
module level1 = 
    let x() = "foo"

[<ReflectedDefinition>]
module level2 = 
    let y() = level1.x()

//[<Test>]
//let ``Compile module into amd module``() =
//    let results = FunScript.CompileModule(gettypedef<level1>)
//    let expected = """foo\
//    """
//    Assert.AreEqual(expected, results)
//
//[<Test>]
//let ``Compile multiple modules into amd modules``() =
//    test.x()

[<Test>]
let ``playing``() =
    check
        <@@
        level2.y()
        @@>
