namespace ConditionCompiler

open FParsec

module Parser = 
    type Identifier = string
    type LogicalOperator = And | Or
    type ComparisonOperator = Equal | NotEqual | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual

    type Expression = 
        | Literal of Types.Value
        | Var of Identifier
        | Comparison of Expression * ComparisonOperator * Expression
        | Logical of Expression * LogicalOperator * Expression
    

    let private pnumliteral: Parser<Expression, unit> =
        let numberFormat = NumberLiteralOptions.AllowFraction
        numberLiteral numberFormat "number"
        |>> fun nl -> Literal(Types.Number(double nl.String))


    let private str = pstring
    let private ws = skipManySatisfy (fun c -> c = ' ' || c = '\t' || c = '\r')
    let private str_ws s = str s .>> ws
    let private str_ws1 s = str s .>> spaces1

    let private lparen = str "(" >>. ws
    let private rparen = str ")" >>. ws
    let private tryBetweenParens p = lparen >>? (p .>>? rparen)

    let private pstringliteral = 
        between (str "\"") (str "\"") (manySatisfy (fun x -> x <> '"'))
        |>> (fun s -> Literal(Types.String(s)))
    
    let private pidentifier = 
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    
    let private pidentifier_ws = pidentifier .>> ws
    let private pvar = pidentifier |>> (fun x -> Var(x))

    let private pvalue = 
        choice [
            pnumliteral; pstringliteral
            attempt pvar
        ]

    
    type Assoc = Associativity

    let private oppc = new OperatorPrecedenceParser<Expression, unit, unit>()
    let private pcomparison = oppc.ExpressionParser
    let private termc = (pvalue .>> ws) <|> tryBetweenParens pcomparison
    oppc.TermParser <- termc
    oppc.AddOperator(InfixOperator("=", ws, 1, Assoc.Left, fun x y -> Comparison(x, Equal, y)))
    oppc.AddOperator(InfixOperator("<>", ws, 1, Assoc.Left, fun x y -> Comparison(x, NotEqual, y)))
    oppc.AddOperator(InfixOperator("<=", ws, 1, Assoc.Left, fun x y -> Comparison(x, LessThanOrEqual, y)))
    oppc.AddOperator(InfixOperator(">=", ws, 1, Assoc.Left, fun x y -> Comparison(x, GreaterThanOrEqual, y)))
    oppc.AddOperator(InfixOperator("<", ws, 1, Assoc.Left, fun x y -> Comparison(x, LessThan, y)))
    oppc.AddOperator(InfixOperator(">", ws, 1, Assoc.Left, fun x y -> Comparison(x, GreaterThan, y)))

    let private oppl = new OperatorPrecedenceParser<Expression, unit, unit>()
    let private plogical = oppl.ExpressionParser
    let private terml = (pcomparison .>> ws) <|> tryBetweenParens plogical
    oppl.TermParser <- terml
    oppl.AddOperator(InfixOperator("&&", ws, 1, Assoc.Left, fun x y -> Logical(x, And, y)))
    oppl.AddOperator(InfixOperator("||", ws, 1, Assoc.Left, fun x y -> Logical(x, Or, y)))

    let private ptest = between (str_ws "(") (str_ws ")") (sepBy plogical (str_ws "&&" <|> str_ws "||"))
    let private final = [plogical;pcomparison] |> List.map attempt |> choice .>> eof

    let Parse (program:string) =
        match run final program with
        | Success(result, _1, _2)   -> 
            result 
        | Failure(errorMsg, e, s) -> failwith errorMsg