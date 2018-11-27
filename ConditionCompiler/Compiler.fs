namespace ConditionCompiler

module Compiler = 
    open System.Linq.Expressions

    /// <summary>
    /// Set up a compiler to use for compiling conditions.
    /// </summary>
    /// <param name="dataBagType">Type of the data bag.</param>
    /// <param name="getDataMethodInfo"><see cref="reference">Method info</see> for getting data from the data bag.</param>
    let CreateCompiler dataBagType getDataMethodInfo =
        let dbparam = System.Linq.Expressions.Expression.Parameter(dataBagType, "dataBagParameter")

        let valueType = typeof<Types.Value>
        let arrayContainsMethodInfo = valueType.GetMethods() |> Array.find (fun l -> l.Name.Equals("ArrayContains"))

        let rec CreateLambdaExpression' dataBagParameter expression = 
            match expression with
            | Parser.Expression.Literal value -> System.Linq.Expressions.Expression.Constant(value) :> System.Linq.Expressions.Expression
            | Parser.Expression.Comparison(leftHandExpression, comparisonOperator, rightHandExpression) ->
                let leftHandCompiled = leftHandExpression |> CreateLambdaExpression' dataBagParameter
                let rightHandCompiled = rightHandExpression |> CreateLambdaExpression' dataBagParameter
                
                (match comparisonOperator with
                | Parser.In -> System.Linq.Expressions.Expression.Equal(System.Linq.Expressions.Expression.Call(arrayContainsMethodInfo, [| leftHandCompiled; rightHandCompiled |]), System.Linq.Expressions.Expression.Constant(true))
                | Parser.Equal -> System.Linq.Expressions.Expression.Equal(leftHandCompiled, rightHandCompiled)
                | Parser.NotEqual -> System.Linq.Expressions.Expression.NotEqual(leftHandCompiled, rightHandCompiled)
                | Parser.LessThan -> System.Linq.Expressions.Expression.LessThan(leftHandCompiled, rightHandCompiled)
                | Parser.GreaterThan -> System.Linq.Expressions.Expression.GreaterThan(leftHandCompiled, rightHandCompiled)
                | Parser.LessThanOrEqual -> System.Linq.Expressions.Expression.LessThanOrEqual(leftHandCompiled, rightHandCompiled)
                | Parser.GreaterThanOrEqual -> System.Linq.Expressions.Expression.GreaterThanOrEqual(leftHandCompiled, rightHandCompiled)) :> System.Linq.Expressions.Expression
            | Parser.Expression.Var varName ->
                let keyParameter = System.Linq.Expressions.Expression.Constant(varName)
                let fetchDataCall = System.Linq.Expressions.Expression.Call(getDataMethodInfo, keyParameter, dataBagParameter)
                fetchDataCall :> System.Linq.Expressions.Expression
            | Parser.Expression.Logical(leftHandExpression, logicalOperator, rightHandExpression) ->
                let leftHandCompiled = leftHandExpression |> CreateLambdaExpression' dataBagParameter
                let rightHandCompiled = rightHandExpression |> CreateLambdaExpression' dataBagParameter
                (match logicalOperator with
                | Parser.And -> System.Linq.Expressions.Expression.And(leftHandCompiled, rightHandCompiled)
                | Parser.Or -> System.Linq.Expressions.Expression.Or(leftHandCompiled, rightHandCompiled)) :> System.Linq.Expressions.Expression
        
        ((CreateLambdaExpression' dbparam), dbparam)

    
    let CompileLambda dataBagParameter expression =
         let lambda = System.Linq.Expressions.Expression.Lambda(expression, dataBagParameter)
         lambda.Compile()
        
