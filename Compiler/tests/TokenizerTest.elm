module TokenizerTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Main
    exposing
        ( Base(..)
        , Id(..)
        , Operator(..)
        , ScientificNumber(..)
        , Sign(..)
        , Token
        , tokenIdToString
        , tokenize
        )
import Natural exposing (Natural)
import Test exposing (Test)


suite : Test
suite =
    Test.fuzz tokens "Generate and tokenize random values" <|
        \generated ->
            let
                rendered : String
                rendered =
                    listToString generated
            in
            tokenize rendered
                -- Drop the first indent for now
                |> List.drop 1
                |> Expect.equal generated


listToString : List Token -> String
listToString list =
    case list of
        [] ->
            ""

        ( n, (T_Indent _) as tokenId ) :: rest ->
            listToStringHelp (String.repeat n " " ++ tokenIdToString tokenId) rest

        [ ( n, tokenId ) ] ->
            String.repeat n " " ++ tokenIdToString tokenId

        ( n, tokenId ) :: xs ->
            listToStringHelp (String.repeat n " " ++ tokenIdToString tokenId ++ " ") xs


listToStringHelp : String -> List Token -> String
listToStringHelp acc list =
    case list of
        [] ->
            acc

        ( _, (T_Indent _) as tokenId ) :: xs ->
            listToStringHelp (acc ++ tokenIdToString tokenId) xs

        ( _, tokenId ) :: xs ->
            listToStringHelp (acc ++ tokenIdToString tokenId ++ " ") xs


tokens : Fuzzer (List Token)
tokens =
    Fuzz.list id |> Fuzz.map (idsToTokens 0 [])


idsToTokens : Int -> List Token -> List Id -> List Token
idsToTokens i acc remaining =
    case remaining of
        [] ->
            List.reverse acc

        token :: rest ->
            let
                length : Int
                length =
                    tokenLength token

                position : Int
                position =
                    case token of
                        T_Indent _ ->
                            i + 1

                        _ ->
                            i
            in
            idsToTokens (i + length + 1) (( position, token ) :: acc) rest


tokenLength : Id -> Int
tokenLength token =
    case token of
        T_Decimal before after ->
            String.length (Natural.toString before)
                + String.length (Natural.toString after)
                + 1

        T_Number Base16 n ->
            String.length (Natural.toHexString n) + 2

        T_Number Base10 n ->
            Natural.toString n |> String.length

        T_Number Base8 n ->
            String.length (Natural.toOctalString n) + 2

        T_Number Base2 n ->
            String.length (Natural.toBinaryString n) + 2

        T_Illegal str ->
            String.length str

        T_Lowercase str ->
            String.length str

        T_Equal ->
            1

        T_Indent indent ->
            indent + 1

        T_Uppercase str ->
            String.length str

        T_Export ->
            6

        T_Import ->
            6

        T_Scientific (IntegerBase n) Positive m ->
            String.length (Natural.toString n)
                + String.length (Natural.toString m)
                + 1

        T_Scientific (IntegerBase n) Negative m ->
            String.length (Natural.toString n)
                + String.length (Natural.toString m)
                + 2

        T_Scientific (DecimalBase n m) Positive x ->
            String.length (Natural.toString n)
                + String.length (Natural.toString m)
                + String.length (Natural.toString x)
                + 2

        T_Scientific (DecimalBase n m) Negative x ->
            String.length (Natural.toString n)
                + String.length (Natural.toString m)
                + String.length (Natural.toString x)
                + 3

        T_BinaryOperator Add ->
            1

        T_BinaryOperator Subtract ->
            1

        T_BinaryOperator Multiply ->
            1

        T_BinaryOperator Divide ->
            1

        T_BinaryOperator Power ->
            1


id : Fuzzer Id
id =
    Fuzz.oneOf
        [ Fuzz.map2 T_Number baseFuzzer natural
        , Fuzz.map2 T_Decimal natural natural
        , Fuzz.map3 T_Scientific scientificBase sign natural
        , Fuzz.map T_Lowercase lowercaseWord
        , Fuzz.map T_Uppercase uppercaseWord
        , Fuzz.constant T_Equal
        , Fuzz.constant T_Import
        , Fuzz.constant T_Export
        , Fuzz.map T_BinaryOperator operatorFuzzer
        ]


scientificBase : Fuzzer ScientificNumber
scientificBase =
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


max : Int
max =
    2147483647


uppercaseWord : Fuzzer String
uppercaseWord =
    Fuzz.map2 String.cons uppercaseChar alphaNum


lowercaseWord : Fuzzer String
lowercaseWord =
    Fuzz.map2 String.cons lowercaseChar alphaNum



-- |> Fuzz.filter (\x -> toKeyword x == Nothing)


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
