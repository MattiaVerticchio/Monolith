module Main exposing (Id(..), Token(..), run, tokenize)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions compile


compile : BackendTask FatalError ()
compile =
    File.rawFile "test.🗿"
        |> BackendTask.allowFatal
        |> BackendTask.map tokenize
        |> BackendTask.andThen (Debug.toString >> Script.log)



-- Tokenization


type Tokenizer e a
    = Continue a (List Char)
    | Stop e


type Token
    = Token Int Id


type Id
    = T_Number Int
    | T_Lowercase String
    | T_Equal
    | T_Indent Int
    | T_Illegal String


tokenize : String -> List Token
tokenize string =
    let
        characters =
            String.foldr (::) [] string

        ( indent, afterIndent ) =
            getIndent characters
    in
    t [ Token 0 (T_Indent indent) ] 0 string afterIndent


t : List Token -> Int -> String -> List Char -> List Token
t acc i src remaining =
    case remaining of
        [] ->
            reverseTo [] acc

        '\n' :: xs ->
            let
                ( indent, afterIndent ) =
                    getIndent xs

                token : Token
                token =
                    Token (i + 1) (T_Indent indent)
            in
            t (token :: acc) (i + 1 + indent) src afterIndent

        ' ' :: xs ->
            t acc (i + 1) src xs

        '=' :: xs ->
            t (Token i T_Equal :: acc) (i + 1) src xs

        x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isLowercase code then
                case lowercase 1 xs of
                    Continue length rest ->
                        let
                            newI : Int
                            newI =
                                i + length

                            chunk : String
                            chunk =
                                String.slice i newI src
                        in
                        t (Token i (T_Lowercase chunk) :: acc) newI src rest

                    Stop length ->
                        let
                            id : String
                            id =
                                String.slice i (i + length) src
                        in
                        stop i id acc

            else if isDigit code then
                case integer (code - 0x30) 1 xs of
                    Continue parsed rest ->
                        let
                            newAcc : List Token
                            newAcc =
                                Token i (T_Number parsed.value) :: acc
                        in
                        t newAcc (i + parsed.length) src rest

                    Stop length ->
                        let
                            illegalToken : String
                            illegalToken =
                                String.slice i (i + length) src
                        in
                        stop i illegalToken acc

            else
                let
                    length : Int
                    length =
                        parseIllegal 1 xs

                    token : String
                    token =
                        String.slice i (i + length) src
                in
                stop i token acc


lowercase : Int -> List Char -> Tokenizer Int Int
lowercase length characters =
    case characters of
        [] ->
            Continue length characters

        x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isAlphanumeric code then
                lowercase (length + 1) xs

            else if isBreaking x then
                Continue length characters

            else
                parseIllegal (length + 1) xs |> Stop


integer :
    Int
    -> Int
    -> List Char
    -> Tokenizer Int { length : Int, value : Int }
integer acc length chars =
    case chars of
        [] ->
            Continue { length = length, value = acc } chars

        x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isDigit code then
                integer (acc * 10 + code - 0x30) (length + 1) xs

            else if isBreaking x then
                Continue { length = length, value = acc } chars

            else
                parseIllegal (length + 1) xs |> Stop


parseIllegal : Int -> List Char -> Int
parseIllegal i chars =
    case chars of
        [] ->
            i

        x :: xs ->
            if isBreaking x then
                i

            else
                parseIllegal (i + 1) xs


stop : Int -> String -> List Token -> List Token
stop i str acc =
    reverseTo [ Token i (T_Illegal str) ] acc


reverseTo : List a -> List a -> List a
reverseTo acc remaining =
    case remaining of
        [] ->
            acc

        x :: xs ->
            reverseTo (x :: acc) xs



-- Helpers


isAlphanumeric : Int -> Bool
isAlphanumeric code =
    (0x61 <= code && code <= 0x7A)
        || (code <= 0x5A && 0x41 <= code)
        || (code <= 0x39 && 0x30 <= code)


isLowercase : Int -> Bool
isLowercase code =
    0x61 <= code && code <= 0x7A


isDigit : Int -> Bool
isDigit code =
    code <= 0x39 && 0x30 <= code


isBreaking : Char -> Bool
isBreaking x =
    case x of
        '=' ->
            True

        '>' ->
            True

        '<' ->
            True

        '+' ->
            True

        '-' ->
            True

        '/' ->
            True

        '*' ->
            True

        '|' ->
            True

        '^' ->
            True

        '\\' ->
            True

        ' ' ->
            True

        '\n' ->
            True

        '(' ->
            True

        ')' ->
            True

        '[' ->
            True

        ']' ->
            True

        '{' ->
            True

        '}' ->
            True

        ',' ->
            True

        _ ->
            False



-- Count the number of leading spaces, also returning the characters after


getIndent : List Char -> ( Int, List Char )
getIndent characters =
    getIndentHelp 0 characters


getIndentHelp : Int -> List Char -> ( Int, List Char )
getIndentHelp acc characters =
    case characters of
        ' ' :: rest ->
            getIndentHelp (acc + 1) rest

        rest ->
            ( acc, rest )
