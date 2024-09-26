module TokenizerTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Main exposing (Id(..), Token, tokenize)
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
            listToStringHelp (String.repeat n " " ++ toString tokenId) rest

        [ ( n, tokenId ) ] ->
            String.repeat n " " ++ toString tokenId

        ( n, tokenId ) :: xs ->
            listToStringHelp (String.repeat n " " ++ toString tokenId ++ " ") xs


listToStringHelp : String -> List Token -> String
listToStringHelp acc list =
    case list of
        [] ->
            acc

        ( _, (T_Indent _) as tokenId ) :: xs ->
            listToStringHelp (acc ++ toString tokenId) xs

        ( _, tokenId ) :: xs ->
            listToStringHelp (acc ++ toString tokenId ++ " ") xs


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
        T_Number n ->
            Natural.toString n |> String.length

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


id : Fuzzer Id
id =
    Fuzz.oneOf
        [ Fuzz.map T_Number natural
        , Fuzz.map T_Lowercase lowercaseWord
        , Fuzz.map T_Uppercase uppercaseWord
        , Fuzz.constant T_Equal
        , Fuzz.constant T_Import
        , Fuzz.constant T_Export
        ]


natural : Fuzzer Natural
natural =
    Fuzz.map Natural.fromSafeInt positiveInt


positiveInt : Fuzzer Int
positiveInt =
    Fuzz.intRange 0 max


rendering : Test
rendering =
    Test.fuzz id "Rendering tests" <|
        \t ->
            let
                rendered : String
                rendered =
                    case t of
                        T_Number n ->
                            Natural.toString n

                        T_Lowercase str ->
                            str

                        T_Uppercase str ->
                            str

                        T_Equal ->
                            "="

                        T_Export ->
                            "export"

                        T_Import ->
                            "import"

                        T_Indent n ->
                            "\n" ++ String.repeat n " "

                        T_Illegal str ->
                            str
            in
            Expect.equal rendered (toString t)


toString : Id -> String
toString t =
    case t of
        T_Number n ->
            Natural.toString n

        T_Illegal str ->
            str

        T_Lowercase str ->
            str

        T_Equal ->
            "="

        T_Indent indent ->
            "\n" ++ String.repeat indent " "

        T_Uppercase str ->
            str

        T_Export ->
            "export"

        T_Import ->
            "import"


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
