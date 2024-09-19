module Main exposing (run)

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
    = T_Number Int Int Int
    | T_Illegal Int Int String


tokenize : String -> List Token
tokenize string =
    String.foldr (::) [] string |> t [] 1 1 string


t : List Token -> Int -> Int -> String -> List Char -> List Token
t acc i j src remaining =
    case remaining of
        [] ->
            acc

        '\n' :: xs ->
            t acc 1 (j + 1) src xs

        ' ' :: xs ->
            t acc (i + 1) j src xs

        x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isDigit code then
                case integer (code - 0x30) 1 xs of
                    Continue parsed rest ->
                        let
                            newAcc : List Token
                            newAcc =
                                T_Number i j parsed.value :: acc
                        in
                        t newAcc (i + parsed.length) j src rest

                    Stop length ->
                        let
                            illegalToken : String
                            illegalToken =
                                String.slice i (i + length) src
                        in
                        stop i j illegalToken acc

            else
                let
                    length : Int
                    length =
                        parseIllegal 1 xs

                    token : String
                    token =
                        String.slice i (i + length) src
                in
                stop i j token acc


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


stop : Int -> Int -> String -> List Token -> List Token
stop i j str acc =
    reverseTo [ T_Illegal i j str ] acc


reverseTo : List a -> List a -> List a
reverseTo acc remaining =
    case remaining of
        [] ->
            acc

        x :: xs ->
            reverseTo (x :: acc) xs



-- Helpers


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
