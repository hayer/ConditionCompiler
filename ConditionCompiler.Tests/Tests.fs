module Tests

open System
open Xunit
open System.Collections.Generic

// simple method for getting data from a dictionary
[<AbstractClass;Sealed>]
type StaticDataBagClass private () =
    inherit obj()
    static member GetDataBagValue (key:string) (dataBag:Dictionary<string, ConditionCompiler.Types.Value>) = dataBag.[key]

// add some test data
let dataBag = new Dictionary<string, ConditionCompiler.Types.Value>() :> IDictionary<string, ConditionCompiler.Types.Value>
dataBag.Add("NR150_0", ConditionCompiler.Types.Number 150.0)
dataBag.Add("NR150_1", ConditionCompiler.Types.Number 150.0)
dataBag.Add("NR100", ConditionCompiler.Types.Number 100.0)
dataBag.Add("HelloStr", ConditionCompiler.Types.String "Hello world")

// create the compiler
let classType = Type.GetType("Tests+StaticDataBagClass")
let methodInfo = classType.GetMethod("GetDataBagValue")
let (compiler, parameterRef) = ConditionCompiler.Compiler.CreateCompiler (dataBag.GetType()) methodInfo
let compile = ConditionCompiler.Compiler.CompileLambda [| parameterRef  |]



[<Fact>]
let ``Number is equal to double`` () =
    let numberCondition0 = "NR150_0 = NR150_0"  |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition1 = "NR150_0 = 150"      |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition2 = "150 = NR150_0"      |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition3 = "NR150_0 = NR150_0"  |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition4 = "NR150_0 = NR150_1"  |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition5 = "NR150_1 <> NR100"   |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(numberCondition0.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition2.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition3.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition4.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition5.DynamicInvoke(dataBag) :?> bool)


[<Fact>]
let ``String is equal to string`` () =
    let stringCondition0 = "\"Hello world\" = HelloStr"        |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let stringCondition1 = "HelloStr <> \"Not Hello world\""   |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let stringCondition2 = "HelloStr = HelloStr"               |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(stringCondition0.DynamicInvoke(dataBag) :?> bool)
    Assert.True(stringCondition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(stringCondition2.DynamicInvoke(dataBag) :?> bool)


[<Fact>]
let ``Variable is equal number && variable1 is equal string`` () =
    let condition1 = "NR100 = 100 && HelloStr = \"Hello world\""        |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "NR100 <> 010 && HelloStr <> \"Not Hello world\""  |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)

[<Fact>]
let ``Number in number array`` () =
    let condition1 = "10 = [1; 40; 10; 30]" |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)