module TokenizerTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Main exposing (Id(..), Token(..))
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
            Main.tokenize rendered
                -- Drop the first indent for now
                |> List.drop 1
                |> Expect.equal generated


listToString : List Token -> String
listToString list =
    case list of
        [] ->
            ""

        (Token _ tokenId) :: xs ->
            listToStringHelp (toString tokenId) xs


listToStringHelp : String -> List Token -> String
listToStringHelp acc list =
    case list of
        [] ->
            acc

        (Token _ tokenId) :: xs ->
            listToStringHelp (acc ++ " " ++ toString tokenId) xs


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
            in
            idsToTokens (i + length + 1) (Token i token :: acc) rest


tokenLength : Id -> Int
tokenLength token =
    case token of
        T_Number n ->
            String.fromInt n |> String.length

        T_Illegal str ->
            String.length str

        T_Lowercase str ->
            String.length str

        T_Equal ->
            1

        T_Indent indent ->
            indent + 1


id : Fuzzer Id
id =
    Fuzz.oneOf
        [ Fuzz.map T_Number <| Fuzz.intRange 0 max
        , Fuzz.map T_Lowercase lowercaseWord
        , Fuzz.constant T_Equal
        ]


toString : Id -> String
toString t =
    case t of
        T_Number n ->
            String.fromInt n

        T_Illegal str ->
            str

        T_Lowercase str ->
            str

        T_Equal ->
            "="

        T_Indent indent ->
            "\n" ++ String.repeat indent " "


max : Int
max =
    2147483647


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
