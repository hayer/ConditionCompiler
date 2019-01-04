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
dataBag.Add("Hello-Str", ConditionCompiler.Types.String "Hello world")

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
    let numberCondition6 = "!(NR150_1 = 100)"   |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition7 = "!(NR150_1 <> 150)"   |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition8 = "1 = 1"   |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let numberCondition9 = "1 <> 1"   |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(numberCondition0.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition2.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition3.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition4.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition5.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition6.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition7.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition8.DynamicInvoke(dataBag) :?> bool)
    Assert.True(numberCondition9.DynamicInvoke(dataBag) :?> bool)


[<Fact>]
let ``String is equal to string`` () =
    let stringCondition0 = "\"Hello world\" = HelloStr"        |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let stringCondition1 = "HelloStr <> \"Not Hello world\""   |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let stringCondition2 = "HelloStr = HelloStr"               |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let stringCondition3 = "Hello-Str = HelloStr"              |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(stringCondition0.DynamicInvoke(dataBag) :?> bool)
    Assert.True(stringCondition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(stringCondition2.DynamicInvoke(dataBag) :?> bool)
    Assert.True(stringCondition3.DynamicInvoke(dataBag) :?> bool)


[<Fact>]
let ``Variable is equal number && variable1 is equal string`` () =
    let condition1 = "NR100 = 100 && HelloStr = \"Hello world\""        |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "NR100 <> 010 && HelloStr <> \"Not Hello world\""  |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)

[<Fact>]
let ``String in string array`` () =
    let condition1 = "HelloStr IN [\"Hello world\"; \"TEST\"; \"HEI\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "HelloStr IN [\"HeLLo wOrLd\"; \"TEST\"; \"HEI\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition3 = "\"TEST\" IN [\"1\"; \"TEST\"; \"HEI\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition3.DynamicInvoke(dataBag) :?> bool)

[<Fact>]
let ``Number in number array`` () =
    let condition1 = "10 IN [1; 40; 10; 30]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "NR100 IN [100; 1; 0]"  |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)

[<Fact>]
let ``String starts with`` () =
    let condition1 = "\"Hei\" =~ \"Hei verden\"" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "HelloStr =~ \"HellO WoRLd this is ME\"" |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)

[<Fact>]
let ``String starts with one or more in array`` () =
    let condition1 = "\"Hei\" =~ [\"asdf\"; \"Nein\"; \"Hei verd\"; \"Might be\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "HelloStr =~ [\"asdf\"; \"Hello world, its me CompilerError\"; \"Not hello world\"; \"Might be\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    
    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)


[<Fact>]
let ``String ends with one or more in array`` () =
    let condition1 = "\"VERD\" ~= [\"asdf\"; \"Nein\"; \"Hei verd\"; \"Might be\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "HelloStr ~= [\"asdf\"; \"Hello world, its me CompilerError\"; \"Not hello world\"; \"Might be\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    
    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)

[<Fact>]
let ``String contains string`` () =
    let condition1 = "\"Word\" ~=~ \"Hello there Mr. Word, are you OK?\"" |> ConditionCompiler.Parser.Parse |> compiler |> compile
    let condition2 = "\"Word\" ~=~ [\"1Excel2\"; \"..3Word..4\"; \"9PaintASD\"]" |> ConditionCompiler.Parser.Parse |> compiler |> compile

    Assert.True(condition1.DynamicInvoke(dataBag) :?> bool)
    Assert.True(condition2.DynamicInvoke(dataBag) :?> bool)