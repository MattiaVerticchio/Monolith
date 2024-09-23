module Main exposing (Id(..), Token(..), run, tokenize)

import BackendTask as Task exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.File as File
import BackendTask.Stream exposing (Error)
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions compile


compile : BackendTask FatalError ()
compile =
    readFolder "src"
        |> Task.andThen readAndParseFiles
        |> Task.andThen
            (List.map fileToString >> String.join "\n" >> Script.log)


type alias Dirent =
    { name : String
    , parentPath : String
    }


readFolder : String -> BackendTask FatalError (List String)
readFolder path =
    Custom.run "readFolder" (Encode.string path) (Decode.list decodeDirent)
        |> Task.map filterFiles
        |> Task.allowFatal


filterFiles : List Dirent -> List String
filterFiles =
    List.filterMap <|
        \dirent ->
            if String.endsWith ".🗿" dirent.name then
                dirent.parentPath ++ "/" ++ dirent.name |> Just

            else
                Nothing


decodeDirent : Decoder Dirent
decodeDirent =
    Decode.map2 Dirent
        (Decode.field "name" Decode.string)
        (Decode.field "parentPath" Decode.string)


readAndParseFiles : List String -> BackendTask FatalError (List File)
readAndParseFiles paths =
    List.map readAndParseFile paths |> Task.combine


readAndParseFile : String -> BackendTask FatalError File
readAndParseFile source =
    File.rawFile source
        |> Task.allowFatal
        |> Task.andThen (tokenize >> parseFile)



-- Tokenize


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



-- Parse


type Parser e a
    = Error e
    | Parsed a (List Token)


type alias File =
    -- The list is reversed
    List Declaration


type alias Declaration =
    { name : String
    , expression : Expression
    , start : Int
    }


type DeclarationParsingError
    = WrongDeclarationSyntax
    | ExpressionParsingError Int String ExpressionParsingError


type Expression
    = Number Int Int


type ExpressionParsingError
    = UnexpectedTokenForExpression Int Id
    | EndOfFileDuringExpression


parseFile : List Token -> BackendTask FatalError File
parseFile tokens =
    case parseDeclarations tokens of
        Parsed file _ ->
            Task.succeed file

        Error e ->
            errorToString e
                |> FatalError.fromString
                |> Task.fail


parseDeclarations : List Token -> Parser DeclarationParsingError (List Declaration)
parseDeclarations tokens =
    parseDeclarationsHelp [] tokens


parseDeclarationsHelp : List Declaration -> List Token -> Parser DeclarationParsingError (List Declaration)
parseDeclarationsHelp acc tokens =
    case tokens of
        [] ->
            Parsed acc []

        [ Token _ (T_Indent _) ] ->
            Parsed acc []

        (Token _ (T_Indent _)) :: (((Token _ (T_Indent _)) :: _) as rest) ->
            parseDeclarationsHelp acc rest

        (Token _ (T_Indent 0)) :: (Token start (T_Lowercase str)) :: (Token _ T_Equal) :: rest ->
            case parseExpression rest of
                Parsed exp afterExp ->
                    parseDeclarationsHelp (Declaration str exp start :: acc) afterExp

                Error e ->
                    ExpressionParsingError start str e |> Error

        (Token _ _) :: _ ->
            Error WrongDeclarationSyntax


parseExpression : List Token -> Parser ExpressionParsingError Expression
parseExpression tokens =
    case tokens of
        (Token i (T_Number n)) :: rest ->
            Parsed (Number i n) rest

        (Token i unexpected) :: _ ->
            UnexpectedTokenForExpression i unexpected |> Error

        [] ->
            Error EndOfFileDuringExpression



-- Format


fileToString : File -> String
fileToString file =
    List.map declarationToString file |> String.join "\n\n"


declarationToString : Declaration -> String
declarationToString declaration =
    declaration.name ++ " = " ++ expressionToString declaration.expression


expressionToString : Expression -> String
expressionToString expression =
    case expression of
        Number _ n ->
            String.fromInt n



-- Error messages


errorToString : DeclarationParsingError -> String
errorToString error =
    case error of
        WrongDeclarationSyntax ->
            "Wrong declaration syntax"

        ExpressionParsingError _ str e ->
            "I’m trying to parse "
                ++ str
                ++ " but I have a problem. "
                ++ expressionParsingErrorToString e


expressionParsingErrorToString : ExpressionParsingError -> String
expressionParsingErrorToString error =
    case error of
        EndOfFileDuringExpression ->
            "The file ended."

        UnexpectedTokenForExpression _ token ->
            "I encountered " ++ describeToken token


describeToken : Id -> String
describeToken id =
    case id of
        T_Number n ->
            "the number " ++ String.fromInt n

        T_Lowercase str ->
            "the word " ++ str

        T_Equal ->
            "the symbol ="

        T_Indent n ->
            "an indentation of " ++ String.fromInt n ++ " spaces"

        T_Illegal str ->
            "something weird: " ++ str
