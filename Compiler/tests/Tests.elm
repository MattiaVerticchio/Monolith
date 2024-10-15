module Tests exposing (..)

import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Main
    exposing
        ( Base(..)
        , Declaration
        , Exports
        , Expression(..)
        , ExpressionParsingError
        , Id(..)
        , Imports
        , Literal(..)
        , Operator(..)
        , Parser(..)
        , ScientificNumber(..)
        , Sign(..)
        , Token
        , exportParsingErrorToString
        , exportsToString
        , expressionParsingErrorToString
        , expressionToString
        , importsToString
        , parseExports
        , parseExpression
        , parseImports
        , toKeyword
        , tokenIdToString
        , tokenize
        )
import Natural exposing (Natural)
import Set
import Test exposing (Test)



{-
   Tokenizer

   The shortest list that gives all the information to check if the tokenizer
   is working correctly has two tokens.

   The input:
       randomSpace1 ++ Token 1 ++ randomSpace2 ++ Token 2

   Should give back:
       Indent randomSpace1, Token 1, Token 2 (with start based on randomSpace2)
-}


type alias Input =
    { start : Int
    , id1 : Id
    , middle : Int
    , id2 : Id
    }


tokenization : Test
tokenization =
    Test.fuzz (Fuzz.map4 Input positiveInt id strictlyPositiveInt id)
        "Generate and tokenize random values"
    <|
        \{ start, id1, middle, id2 } ->
            let
                renderedMiddle : String
                renderedMiddle =
                    case id1 of
                        T_Indent _ ->
                            ""

                        _ ->
                            String.repeat middle " "

                rendered : String
                rendered =
                    String.repeat start " "
                        ++ tokenIdToString id1
                        ++ renderedMiddle
                        ++ tokenIdToString id2

                expected : List Token
                expected =
                    [ ( 0, T_Indent start )
                    , ( start, id1 )
                    , ( start
                            + String.length (tokenIdToString id1)
                            + String.length renderedMiddle
                      , id2
                      )
                    ]

                tokenized : List Token
                tokenized =
                    tokenize rendered
            in
            Expect.equal expected tokenized



-- if tokenized == expected then
--     Expect.pass
-- else
--     "Generated:\n"
--         ++ rendered
--         ++ "\n\nTokenized:\n"
--         ++ Debug.toString tokenized
--         |> Expect.fail
{-
   Parser
-}


parsingExports : Test
parsingExports =
    Test.fuzz exportsFuzzer "Generating and parsing random exports" <|
        \generated ->
            case exportsToString generated |> tokenize |> parseExports of
                Error e ->
                    exportParsingErrorToString e |> Expect.fail

                Parsed parsed _ ->
                    Expect.equalSets generated parsed


parsingImports : Test
parsingImports =
    Test.fuzz importsFuzzer "Generating and parsing random imports" <|
        \generated ->
            case importsToString generated |> tokenize |> parseImports of
                Error _ ->
                    Expect.fail "Failed parsing"

                Parsed parsed _ ->
                    Expect.equalDicts generated parsed


parsingExpressions : Test
parsingExpressions =
    Test.fuzz expressionFuzzer "Generating and parsing random expressions" <|
        \generated ->
            let
                rendered : String
                rendered =
                    expressionToString generated

                tokenized : List Token
                tokenized =
                    tokenize rendered

                parser : Parser ExpressionParsingError Expression
                parser =
                    parseExpression 0 tokenized
            in
            case parser of
                Error e ->
                    expressionParsingErrorToString e |> Expect.fail

                Parsed parsedExpression _ ->
                    if areExpressionsOk generated parsedExpression then
                        Expect.pass

                    else
                        "Generated:\n"
                            ++ rendered
                            ++ "\nParsed:\n"
                            ++ expressionToString parsedExpression
                            |> Expect.fail


parsingNonIndentedExpressions : Test
parsingNonIndentedExpressions =
    Test.fuzz expressionFuzzer
        "Generating and parsing random expressions that are not indented enough"
    <|
        \generated ->
            let
                rendered : String
                rendered =
                    "  " ++ expressionToString generated

                tokenized : List Token
                tokenized =
                    tokenize rendered

                parser : Parser ExpressionParsingError Expression
                parser =
                    parseExpression 4 tokenized
            in
            case parser of
                Error _ ->
                    Expect.pass

                Parsed _ _ ->
                    Expect.fail "This test should not parse any expression"


areExpressionsOk : Expression -> Expression -> Bool
areExpressionsOk a b =
    case ( a, b ) of
        ( Literal _ litA, Literal _ litB ) ->
            litA == litB

        ( Binary opA leftA rightA, Binary opB leftB rightB ) ->
            (opA == opB)
                && areExpressionsOk leftA leftB
                && areExpressionsOk rightA rightB

        _ ->
            False


moduleNameFuzzer : Fuzzer String
moduleNameFuzzer =
    uppercaseWord


exportsFuzzer : Fuzzer Exports
exportsFuzzer =
    Fuzz.listOfLengthBetween 0 2 lowercaseWord |> Fuzz.map Set.fromList


importsFuzzer : Fuzzer Imports
importsFuzzer =
    Fuzz.map2 Tuple.pair
        uppercaseWord
        (Fuzz.listOfLengthBetween 0 2 lowercaseWord |> Fuzz.map Set.fromList)
        |> Fuzz.listOfLengthBetween 0 2
        |> Fuzz.map Dict.fromList


declarationsFuzzer : Fuzzer (List Declaration)
declarationsFuzzer =
    Fuzz.map3 Declaration lowercaseWord expressionFuzzer (Fuzz.constant 0)
        |> Fuzz.listOfLengthBetween 0 2


expressionFuzzer : Fuzzer Expression
expressionFuzzer =
    expressionFuzzerHelper 0


expressionFuzzerHelper : Int -> Fuzzer Expression
expressionFuzzerHelper i =
    let
        limit : Int
        limit =
            2

        rec : Fuzzer Expression
        rec =
            Fuzz.lazy <| \() -> expressionFuzzerHelper (i + 1)
    in
    if i > limit then
        Fuzz.oneOf
            [ Fuzz.map2 Literal (Fuzz.constant 0) literalFuzzer ]

    else
        Fuzz.oneOf
            [ Fuzz.map2 Literal (Fuzz.constant 0) literalFuzzer
            , Fuzz.map3 Binary operatorFuzzer rec rec
            ]


literalFuzzer : Fuzzer Literal
literalFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Number baseFuzzer natural
        , Fuzz.map2 Decimal natural natural
        , Fuzz.map3 Scientific scientificNumber sign natural
        ]


id : Fuzzer Id
id =
    Fuzz.oneOf
        [ Fuzz.map T_Literal literalFuzzer
        , Fuzz.map T_Lowercase lowercaseWord
        , Fuzz.map T_Uppercase uppercaseWord
        , Fuzz.constant T_Equal
        , Fuzz.constant T_Import
        , Fuzz.constant T_Export
        , Fuzz.map T_BinaryOperator operatorFuzzer
        , Fuzz.constant T_OpenParenthesis
        , Fuzz.constant T_CloseParenthesis
        , Fuzz.map T_Indent positiveInt
        ]


scientificNumber : Fuzzer ScientificNumber
scientificNumber =
    Fuzz.oneOf
        [ Fuzz.map IntegerBase natural
        , Fuzz.map2 DecimalBase natural natural
        ]


sign : Fuzzer Sign
sign =
    Fuzz.oneOfValues [ Positive, Negative ]


baseFuzzer : Fuzzer Base
baseFuzzer =
    Fuzz.oneOfValues
        [ Base16
        , Base10
        , Base8
        , Base2
        ]


operatorFuzzer : Fuzzer Operator
operatorFuzzer =
    Fuzz.oneOfValues
        [ Add
        , Subtract
        , Multiply
        , Divide
        , Power
        ]


natural : Fuzzer Natural
natural =
    Fuzz.map Natural.fromSafeInt positiveInt


positiveInt : Fuzzer Int
positiveInt =
    Fuzz.intRange 0 9999


strictlyPositiveInt : Fuzzer Int
strictlyPositiveInt =
    Fuzz.intRange 1 9999


max : Int
max =
    2147483647


uppercaseWord : Fuzzer String
uppercaseWord =
    Fuzz.map2 String.cons uppercaseChar alphaNum


lowercaseWord : Fuzzer String
lowercaseWord =
    Fuzz.map2 String.cons lowercaseChar alphaNum
        |> Fuzz.filter (\x -> toKeyword x == Nothing)


alphaNum : Fuzzer String
alphaNum =
    Fuzz.frequency
        [ ( 26, lowercaseChar )
        , ( 26, uppercaseChar )
        , ( 10, digitChar )
        ]
        |> Fuzz.listOfLengthBetween 1 5
        |> Fuzz.map String.fromList


uppercaseChar : Fuzzer Char
uppercaseChar =
    Fuzz.intRange 65 90 |> Fuzz.map Char.fromCode


lowercaseChar : Fuzzer Char
lowercaseChar =
    Fuzz.intRange 97 122 |> Fuzz.map Char.fromCode


digitChar : Fuzzer Char
digitChar =
    Fuzz.intRange 48 57 |> Fuzz.map Char.fromCode
