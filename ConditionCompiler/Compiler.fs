﻿namespace ConditionCompiler

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
        let arrayContainsMethodInfo =    valueType.GetMethods() |> Array.find (fun l -> l.Name.Equals("ArrayContains"))
        let stringStartsWithMethodInfo = valueType.GetMethods() |> Array.find (fun l -> l.Name.Equals("StartsWith"))
        let stringEndsWithMethodInfo =   valueType.GetMethods() |> Array.find (fun l -> l.Name.Equals("EndsWith"))
        let stringContainsMethodInfo =   valueType.GetMethods() |> Array.find (fun l -> l.Name.Equals("Contains"))

        let trueExpr = System.Linq.Expressions.Expression.Constant(true)
        let falseExpr = System.Linq.Expressions.Expression.Constant(false)

        let rec CreateLambdaExpression' dataBagParameter expression = 
            match expression with
            | Parser.Expression.Literal value -> System.Linq.Expressions.Expression.Constant(value) :> System.Linq.Expressions.Expression
            | Parser.Expression.Negate innerExpr -> System.Linq.Expressions.Expression.Equal(innerExpr |> CreateLambdaExpression' dataBagParameter, falseExpr) :> System.Linq.Expressions.Expression
            | Parser.Expression.Comparison(leftHandExpression, comparisonOperator, rightHandExpression) ->
                let leftHandCompiled = leftHandExpression |> CreateLambdaExpression' dataBagParameter
                let rightHandCompiled = rightHandExpression |> CreateLambdaExpression' dataBagParameter
                // check for shortcut
                let shorthandExpr = System.Linq.Expressions.Expression.Equal(trueExpr, trueExpr) :> System.Linq.Expressions.Expression
                match (leftHandExpression, comparisonOperator, rightHandExpression) with
                | Parser.Expression.Literal v1, Parser.ComparisonOperator.Equal, Parser.Expression.Literal v2 when v1 = v2 -> shorthandExpr
                | Parser.Expression.Literal v1, Parser.ComparisonOperator.NotEqual, Parser.Expression.Literal v2 when v1 <> v2 -> shorthandExpr
                | Parser.Expression.Literal v1, Parser.ComparisonOperator.LessThan, Parser.Expression.Literal v2 when v1 < v2 -> shorthandExpr
                | Parser.Expression.Literal v1, Parser.ComparisonOperator.GreaterThan, Parser.Expression.Literal v2 when v1 > v2 -> shorthandExpr
                | Parser.Expression.Literal v1, Parser.ComparisonOperator.LessThanOrEqual, Parser.Expression.Literal v2 when v1 <= v2 -> shorthandExpr
                | Parser.Expression.Literal v1, Parser.ComparisonOperator.GreaterThanOrEqual, Parser.Expression.Literal v2 when v1 >= v2 -> shorthandExpr
                | _ ->
                    (match comparisonOperator with
                    | Parser.Contains   -> System.Linq.Expressions.Expression.Equal(System.Linq.Expressions.Expression.Call(stringContainsMethodInfo,   [| leftHandCompiled; rightHandCompiled |]), trueExpr)
                    | Parser.EndsWith   -> System.Linq.Expressions.Expression.Equal(System.Linq.Expressions.Expression.Call(stringEndsWithMethodInfo,   [| leftHandCompiled; rightHandCompiled |]), trueExpr)
                    | Parser.StartsWith -> System.Linq.Expressions.Expression.Equal(System.Linq.Expressions.Expression.Call(stringStartsWithMethodInfo, [| leftHandCompiled; rightHandCompiled |]), trueExpr)
                    | Parser.In         -> System.Linq.Expressions.Expression.Equal(System.Linq.Expressions.Expression.Call(arrayContainsMethodInfo,    [| leftHandCompiled; rightHandCompiled |]), trueExpr)
                    | Parser.Equal      -> System.Linq.Expressions.Expression.Equal(leftHandCompiled, rightHandCompiled)
                    | Parser.NotEqual   -> System.Linq.Expressions.Expression.NotEqual(leftHandCompiled, rightHandCompiled)
                    | Parser.LessThan   -> System.Linq.Expressions.Expression.LessThan(leftHandCompiled, rightHandCompiled)
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
        
