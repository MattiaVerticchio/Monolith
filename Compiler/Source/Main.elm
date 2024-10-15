module Main exposing (..)

import BackendTask as Task exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.File as File
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Natural exposing (Natural)
import Pages.Script as Script exposing (Script)
import Set exposing (Set)



{-
   Pipeline:
       read the files
        |> tokenize
        |> parse + check for duplicated functions
        |> check for cyclic imports
        |> topological sort
        |> type checking
        |> conversion
        |> emit files
-}


run : Script
run =
    Script.withoutCliOptions compile


extension : String
extension =
    ".🗿"


compile : BackendTask FatalError ()
compile =
    getContent path
        |> Task.map filterFiles
        |> Task.andThen readFiles
        |> Task.map tokenizeFiles
        |> Task.andThen parseFiles
        |> Task.andThen duplicatesAndExports
        -- |> Task.andThen checkCyclicImports
        -- |> Task.map topologicalSort
        -- |> Task.andThen typeCheck
        -- |> Task.map transformToJs
        -- |> Task.andThen emitFile
        |> Task.andThen (Debug.toString >> Script.log)



-- Get the content of the source directory


path : String
path =
    "src"


getContent : String -> BackendTask FatalError (List Dirent)
getContent folder =
    Custom.run "readFolder" (Encode.string folder) (Decode.list decodeDirent)
        |> Task.allowFatal



-- Keep files that end with the proper extension


type alias Dirent =
    { name : String
    , parentPath : String
    }


filterFiles : List Dirent -> List String
filterFiles =
    List.filterMap <|
        \dirent ->
            if String.endsWith extension dirent.name then
                dirent.parentPath ++ "/" ++ dirent.name |> Just

            else
                Nothing


decodeDirent : Decoder Dirent
decodeDirent =
    Decode.map2 Dirent
        (Decode.field "name" Decode.string)
        (Decode.field "parentPath" Decode.string)



-- Read the files


type alias RawFile =
    { name : String
    , content : String
    }


readFiles : List String -> BackendTask FatalError (List RawFile)
readFiles files =
    List.map
        (\name ->
            File.rawFile name
                |> Task.map (\content -> { name = name, content = content })
        )
        files
        |> Task.combine
        |> Task.allowFatal



-- Tokenize the files


type alias TokenizedFile =
    { name : String
    , tokens : List Token
    }


tokenizeFiles : List RawFile -> List TokenizedFile
tokenizeFiles =
    List.map <| \{ name, content } -> { name = name, tokens = tokenize content }



-- Parse the tokenized files


type alias ParsedFile =
    { name : String
    , exports : Exports
    , imports : Imports
    , declarations :
        -- The list is reversed
        List Declaration
    }


parseFiles : List TokenizedFile -> BackendTask FatalError (List ParsedFile)
parseFiles tokenized =
    case
        List.foldl
            (\{ name, tokens } ( errors, ok ) ->
                case parseFileResult name tokens of
                    Err e ->
                        ( e :: errors, ok )

                    Ok parsed ->
                        ( errors, parsed :: ok )
            )
            ( [], [] )
            tokenized
    of
        ( [], parsed ) ->
            Task.succeed parsed

        ( errors, _ ) ->
            String.join "\n" errors |> FatalError.fromString |> Task.fail



-- Check the parsed files for function duplicates and undeclared exports


type alias UnduplicatedFile =
    { name : String
    , imports : Imports
    , functions :
        Dict
            String
            { expression : Expression
            , start : Int
            , isExposed : Bool
            }
    }


duplicatesAndExports : List ParsedFile -> BackendTask FatalError (List UnduplicatedFile)
duplicatesAndExports files =
    case
        List.foldl
            (\file ( errors, result ) ->
                case
                    checkForDuplicatedDeclarations file.exports
                        Dict.empty
                        Dict.empty
                        file.declarations
                of
                    Err duplicates ->
                        ( { name = file.name
                          , duplicates = duplicates
                          }
                            :: errors
                        , result
                        )

                    Ok ok ->
                        ( errors
                        , { name = file.name
                          , imports = file.imports
                          , functions = ok
                          }
                            :: result
                        )
            )
            ( [], [] )
            files
    of
        ( [], result ) ->
            Task.succeed result

        ( errors, _ ) ->
            duplicatesToErrorString errors
                |> FatalError.fromString
                |> Task.fail


duplicatesToErrorString : List { name : String, duplicates : ( Dict String Int, Exports ) } -> String
duplicatesToErrorString errors =
    List.foldl
        (\file mainAcc ->
            let
                ( duplicates, exports ) =
                    file.duplicates

                duplicatesError : String
                duplicatesError =
                    if Dict.isEmpty duplicates then
                        ""

                    else
                        Dict.foldl (\k _ acc -> acc ++ "\n    " ++ k)
                            "\n  Duplicated functions:"
                            duplicates

                exportsError : String
                exportsError =
                    if Set.isEmpty exports then
                        ""

                    else
                        Set.foldl (\k acc -> acc ++ "\n    " ++ k)
                            "\n  Undeclared exports:"
                            exports
            in
            mainAcc
                ++ "\n"
                ++ file.name
                ++ "has:"
                ++ duplicatesError
                ++ exportsError
        )
        ""
        errors


checkForDuplicatedDeclarations :
    Exports
    -> Dict String Int
    ->
        Dict
            String
            { expression : Expression
            , start : Int
            , isExposed : Bool
            }
    -> List Declaration
    ->
        Result
            ( Dict String Int, Exports )
            (Dict
                String
                { expression : Expression
                , start : Int
                , isExposed : Bool
                }
            )
checkForDuplicatedDeclarations exports duplicates declarations remaining =
    case remaining of
        [] ->
            if Dict.isEmpty duplicates && Set.isEmpty exports then
                Ok declarations

            else
                Err ( duplicates, exports )

        x :: xs ->
            case Dict.get x.name declarations of
                Nothing ->
                    let
                        newDeclarations :
                            Dict
                                String
                                { expression : Expression
                                , start : Int
                                , isExposed : Bool
                                }
                        newDeclarations =
                            Dict.insert x.name
                                { expression = x.expression
                                , start = x.start
                                , isExposed = Set.member x.name exports
                                }
                                declarations
                    in
                    checkForDuplicatedDeclarations (Set.remove x.name exports)
                        duplicates
                        newDeclarations
                        xs

                Just _ ->
                    let
                        counter : Int
                        counter =
                            case Dict.get x.name duplicates of
                                Nothing ->
                                    2

                                Just n ->
                                    n + 1

                        newDuplicates : Dict String Int
                        newDuplicates =
                            Dict.insert x.name counter duplicates
                    in
                    checkForDuplicatedDeclarations exports
                        newDuplicates
                        declarations
                        xs


readAndParseFile : String -> BackendTask FatalError ParsedFile
readAndParseFile filePath =
    let
        name : String
        name =
            String.split "/" filePath
                |> lastElement
                |> Maybe.withDefault ""
                |> String.dropRight (String.length extension)
    in
    File.rawFile filePath
        |> Task.allowFatal
        |> Task.andThen (tokenize >> parseFile name)


lastElement : List a -> Maybe a
lastElement list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: xs ->
            lastElement xs



-- Tokenize


type Tokenizer e a
    = Continue a (List Char)
    | Stop e


type alias Token =
    ( Int, Id )


type Id
    = T_Literal Literal
      -- before.after : 3.14 -> before = 3, after = 14
    | T_Lowercase String
    | T_Uppercase String
    | T_Equal
    | T_Export
    | T_Import
    | T_OpenParenthesis
    | T_CloseParenthesis
    | T_Indent Int
    | T_Illegal String
    | T_BinaryOperator Operator


type ScientificNumber
    = IntegerBase Natural
    | DecimalBase Natural Natural


type Sign
    = Positive
    | Negative


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Power


type Base
    = Base16
    | Base10
    | Base8
    | Base2


tokenize : String -> List Token
tokenize string =
    let
        characters : List Char
        characters =
            String.foldr (::) [] string

        ( indent, afterIndent ) =
            getIndent characters
    in
    t [ ( 0, T_Indent indent ) ] indent string afterIndent


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
                    ( i, T_Indent indent )

                afterSpace : Int
                afterSpace =
                    i + indent + 1
            in
            t (token :: acc) afterSpace src afterIndent

        ' ' :: xs ->
            t acc (i + 1) src xs

        '=' :: xs ->
            t (( i, T_Equal ) :: acc) (i + 1) src xs

        '(' :: xs ->
            t (( i, T_OpenParenthesis ) :: acc) (i + 1) src xs

        ')' :: xs ->
            t (( i, T_CloseParenthesis ) :: acc) (i + 1) src xs

        '+' :: xs ->
            t (( i, T_BinaryOperator Add ) :: acc) (i + 1) src xs

        '-' :: xs ->
            t (( i, T_BinaryOperator Subtract ) :: acc) (i + 1) src xs

        '*' :: xs ->
            t (( i, T_BinaryOperator Multiply ) :: acc) (i + 1) src xs

        '/' :: xs ->
            t (( i, T_BinaryOperator Divide ) :: acc) (i + 1) src xs

        '^' :: xs ->
            t (( i, T_BinaryOperator Power ) :: acc) (i + 1) src xs

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

                            id : Id
                            id =
                                case toKeyword chunk of
                                    Just key ->
                                        key

                                    Nothing ->
                                        T_Lowercase chunk
                        in
                        t (( i, id ) :: acc) newI src rest

                    Stop length ->
                        let
                            id : String
                            id =
                                String.slice i (i + length) src
                        in
                        stop i id acc

            else if isUppercase code then
                case chompUppercase i 1 xs src of
                    Continue ( newI, id ) rest ->
                        t (( i, id ) :: acc) newI src rest

                    Stop length ->
                        let
                            id : String
                            id =
                                String.slice i (i + length) src
                        in
                        stop i id acc

            else if isDigit code then
                let
                    ( end, afterEnd ) =
                        untilDigitBreaking (i + 1) xs

                    length : Int
                    length =
                        end - i

                    chunk : String
                    chunk =
                        String.slice i end src

                    maybeLiteral : Maybe Literal
                    maybeLiteral =
                        if String.startsWith "0b" chunk || String.startsWith "0B" chunk then
                            String.slice 2 length chunk
                                |> Natural.fromBinaryString
                                |> Maybe.map (Number Base2)

                        else if String.startsWith "0o" chunk || String.startsWith "0O" chunk then
                            String.slice 2 length chunk
                                |> Natural.fromOctalString
                                |> Maybe.map (Number Base8)

                        else if String.startsWith "0x" chunk || String.startsWith "0X" chunk then
                            String.slice 2 length chunk
                                |> Natural.fromHexString
                                |> Maybe.map (Number Base16)

                        else
                            case String.split "e" chunk of
                                [ before, after ] ->
                                    let
                                        ( sign, newAfter ) =
                                            if String.startsWith "-" after then
                                                ( Negative
                                                , String.dropLeft 1 after
                                                )

                                            else
                                                ( Positive, after )
                                    in
                                    Maybe.map3 Scientific
                                        (parseScientificNumber before)
                                        (Just sign)
                                        (Natural.fromDecimalString newAfter)

                                _ ->
                                    case parseScientificNumber chunk of
                                        Just (IntegerBase n) ->
                                            Number Base10 n |> Just

                                        Just (DecimalBase n m) ->
                                            Decimal n m |> Just

                                        _ ->
                                            Nothing
                in
                case maybeLiteral of
                    Just literal ->
                        t (( i, T_Literal literal ) :: acc) end src afterEnd

                    Nothing ->
                        let
                            token : String
                            token =
                                String.slice i (i + length) src
                        in
                        stop i token acc

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


parseScientificNumber : String -> Maybe ScientificNumber
parseScientificNumber string =
    case String.split "." string of
        [ before, after ] ->
            case ( Natural.fromString before, Natural.fromString after ) of
                ( Just b, Just a ) ->
                    DecimalBase b a |> Just

                _ ->
                    Nothing

        _ ->
            case Natural.fromBaseBString 10 string of
                Just integer ->
                    IntegerBase integer |> Just

                _ ->
                    Nothing


untilDigitBreaking : Int -> List Char -> ( Int, List Char )
untilDigitBreaking i characters =
    case characters of
        [] ->
            ( i, characters )

        'e' :: '-' :: xs ->
            untilDigitBreaking (i + 2) xs

        x :: xs ->
            if isBreaking x then
                ( i, characters )

            else
                untilDigitBreaking (i + 1) xs


untilBreaking : Int -> List Char -> ( Int, List Char )
untilBreaking i characters =
    case characters of
        [] ->
            ( i, characters )

        x :: xs ->
            if isBreaking x then
                ( i, characters )

            else
                untilBreaking (i + 1) xs


toKeyword : String -> Maybe Id
toKeyword str =
    case str of
        "export" ->
            Just T_Export

        "import" ->
            Just T_Import

        _ ->
            Nothing


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
    reverseTo [ ( i, T_Illegal str ) ] acc


reverseTo : List a -> List a -> List a
reverseTo acc remaining =
    case remaining of
        [] ->
            acc

        x :: xs ->
            reverseTo (x :: acc) xs


chompUppercase : Int -> Int -> List Char -> String -> Tokenizer Int ( Int, Id )
chompUppercase start length characters src =
    case characters of
        [] ->
            let
                newI : Int
                newI =
                    start + length

                id : String
                id =
                    String.slice start newI src
            in
            Continue ( newI, T_Uppercase id ) characters

        '.' :: x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isLowercase code then
                case lowercase 1 xs of
                    Continue lowercaseLength afterLowercase ->
                        let
                            moduleNameEnd : Int
                            moduleNameEnd =
                                start + length

                            lowercaseStart : Int
                            lowercaseStart =
                                moduleNameEnd + 1

                            newI : Int
                            newI =
                                lowercaseStart + lowercaseLength

                            moduleName : String
                            moduleName =
                                String.slice start moduleNameEnd src

                            functionName =
                                String.slice lowercaseStart newI src
                        in
                        Continue ( newI, T_Lowercase {- moduleName -} functionName )
                            afterLowercase

                    Stop e ->
                        Stop e

            else
                parseIllegal (length + 2) xs |> Stop

        x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isAlphanumeric code then
                chompUppercase start (length + 1) xs src

            else if isBreaking x then
                let
                    newI : Int
                    newI =
                        start + length

                    id : String
                    id =
                        String.slice start newI src
                in
                Continue ( newI, T_Uppercase id ) characters

            else
                parseIllegal (length + 1) xs |> Stop


tokenIdToString : Id -> String
tokenIdToString id =
    case id of
        T_Literal literal ->
            literalToString literal

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

        T_BinaryOperator operator ->
            operatorToString operator

        T_OpenParenthesis ->
            "("

        T_CloseParenthesis ->
            ")"



-- Helpers


isAlphanumeric : Int -> Bool
isAlphanumeric code =
    (0x61 <= code && code <= 0x7A)
        || (code <= 0x5A && 0x41 <= code)
        || (code <= 0x39 && 0x30 <= code)


isUppercase : Int -> Bool
isUppercase code =
    code <= 0x5A && 0x41 <= code


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


type alias Exports =
    Set String


type alias Imports =
    Dict String (Set String)


type alias Declaration =
    { name : String
    , expression : Expression
    , start : Int
    }


type DeclarationParsingError
    = UnexpectedToken Token
    | ExpressionParsingError Int String ExpressionParsingError
    | DeclarationMustBeLowercase Token
    | ExpectingEqual Token


type Expression
    = Literal Int Literal
    | Binary Operator Expression Expression


type Literal
    = Number Base Natural
    | Decimal Natural Natural
    | Scientific ScientificNumber Sign Natural


type ExpressionParsingError
    = UnexpectedTokenForExpression Int Id
    | EndOfFileDuringExpression
    | ExpectingIndentAtLeast { position : Int, minIndent : Int, found : Int }
    | MissingClosingParenthesis Int


parseFile : String -> List Token -> BackendTask FatalError ParsedFile
parseFile name tokens =
    case parseFileResult name tokens of
        Err e ->
            FatalError.fromString e |> Task.fail

        Ok file ->
            Task.succeed file


parseFileResult : String -> List Token -> Result String ParsedFile
parseFileResult name tokens =
    case parseExports tokens of
        Error e ->
            exportParsingErrorToString e |> Err

        Parsed exports afterExports ->
            case parseImports afterExports of
                Error e ->
                    Err e

                Parsed imports afterImports ->
                    case parseDeclarations afterImports of
                        Parsed declarations _ ->
                            ParsedFile name exports imports declarations |> Ok

                        Error e ->
                            errorToString e |> Err


type ExportParsingError
    = ExpectingAFunctionName Token


parseExports : List Token -> Parser ExportParsingError Exports
parseExports tokens =
    case tokens of
        ( _, T_Indent _ ) :: ((( _, T_Indent _ ) :: _) as rest) ->
            parseExports rest

        ( _, T_Indent 0 ) :: ( _, T_Export ) :: afterKeyword ->
            parseExportList Set.empty afterKeyword

        _ ->
            Parsed Set.empty tokens


parseExportList : Exports -> List Token -> Parser ExportParsingError Exports
parseExportList exports tokens =
    case tokens of
        ( _, T_Indent _ ) :: ((( _, T_Indent _ ) :: _) as rest) ->
            parseExportList exports rest

        ( _, T_Indent n ) :: ( _, T_Lowercase f ) :: rest ->
            if n > 0 then
                parseExportList (Set.insert f exports) rest

            else
                Parsed exports tokens

        ( _, T_Indent n ) :: unexpected :: _ ->
            if n > 0 then
                ExpectingAFunctionName unexpected |> Error

            else
                Parsed exports tokens

        _ ->
            Parsed exports tokens


parseImports : List Token -> Parser e Imports
parseImports tokens =
    case tokens of
        ( _, T_Indent _ ) :: ((( _, T_Indent _ ) :: _) as rest) ->
            parseImports rest

        ( _, T_Indent 0 ) :: ( _, T_Import ) :: rest ->
            parseImportList Dict.empty rest

        _ ->
            Parsed Dict.empty tokens


parseImportList : Imports -> List Token -> Parser e Imports
parseImportList imports tokens =
    case tokens of
        ( _, T_Indent n ) :: ( _, T_Uppercase m ) :: afterModule ->
            if n > 0 then
                let
                    acc : Set String
                    acc =
                        case Dict.get m imports of
                            Nothing ->
                                Set.empty

                            Just exposed ->
                                exposed
                in
                case parseExposedList n acc afterModule of
                    Error e ->
                        Error e

                    Parsed exposed afterExposed ->
                        parseImportList (Dict.insert m exposed imports)
                            afterExposed

            else
                Parsed imports tokens

        _ ->
            Parsed imports tokens


parseExposedList : Int -> Set String -> List Token -> Parser e (Set String)
parseExposedList indent exposed tokens =
    case tokens of
        ( _, T_Indent _ ) :: ((( _, T_Indent _ ) :: _) as rest) ->
            parseExposedList indent exposed rest

        ( _, T_Indent n ) :: ( _, T_Lowercase f ) :: afterFunction ->
            if n > indent then
                parseExposedList indent (Set.insert f exposed) afterFunction

            else
                Parsed exposed tokens

        _ ->
            Parsed exposed tokens


exportParsingErrorToString : ExportParsingError -> String
exportParsingErrorToString error =
    case error of
        ExpectingAFunctionName ( _, id ) ->
            "I’m trying to parse an exported function but I found "
                ++ describeToken id


parseDeclarations : List Token -> Parser DeclarationParsingError (List Declaration)
parseDeclarations tokens =
    parseDeclarationsHelp [] tokens


parseDeclarationsHelp : List Declaration -> List Token -> Parser DeclarationParsingError (List Declaration)
parseDeclarationsHelp acc tokens =
    case tokens of
        [] ->
            Parsed acc []

        [ ( _, T_Indent _ ) ] ->
            Parsed acc []

        ( _, T_Indent _ ) :: ((( _, T_Indent _ ) :: _) as rest) ->
            parseDeclarationsHelp acc rest

        ( _, T_Indent 0 ) :: ( start, T_Lowercase str ) :: ( _, T_Equal ) :: afterEqual ->
            case parseExpression 1 afterEqual of
                Parsed exp afterExp ->
                    parseDeclarationsHelp
                        (Declaration str exp start :: acc)
                        afterExp

                Error e ->
                    ExpressionParsingError start str e |> Error

        ( _, T_Indent 0 ) :: ( _, T_Lowercase _ ) :: found :: _ ->
            ExpectingEqual found |> Error

        ( _, T_Indent 0 ) :: unexpected :: _ ->
            DeclarationMustBeLowercase unexpected |> Error

        ( _, T_Indent _ ) :: unexpected :: _ ->
            UnexpectedToken unexpected |> Error

        unexpected :: _ ->
            UnexpectedToken unexpected |> Error


parseExpression : Int -> List Token -> Parser ExpressionParsingError Expression
parseExpression indent tokens =
    pratt indent 0 tokens


pratt : Int -> Int -> List Token -> Parser ExpressionParsingError Expression
pratt indent limit tokens =
    case parseLeftExpression indent tokens of
        Error e ->
            Error e

        Parsed left afterLeft ->
            prattLoop indent limit left afterLeft


parseLeftExpression : Int -> List Token -> Parser ExpressionParsingError Expression
parseLeftExpression indent tokens =
    case tokens of
        ( _, T_Indent _ ) :: ((( _, T_Indent _ ) :: _) as rest) ->
            parseLeftExpression indent rest

        [ ( _, T_Indent _ ) ] ->
            Error EndOfFileDuringExpression

        ( i, T_Indent found ) :: rest ->
            if found >= indent then
                parseLeftExpressionAfterIndent rest

            else
                ExpectingIndentAtLeast
                    { position = i
                    , minIndent = indent
                    , found = found
                    }
                    |> Error

        [] ->
            Error EndOfFileDuringExpression

        _ ->
            parseLeftExpressionAfterIndent tokens


parseLeftExpressionAfterIndent tokens =
    case tokens of
        ( i, T_Literal literal ) :: rest ->
            let
                expression : Expression
                expression =
                    Literal i literal
            in
            Parsed expression rest

        ( i, T_OpenParenthesis ) :: rest ->
            case parseExpression 0 rest of
                Error e ->
                    Error e

                Parsed inside (( _, T_CloseParenthesis ) :: afterClosing) ->
                    Parsed inside afterClosing

                Parsed _ _ ->
                    MissingClosingParenthesis i |> Error

        ( i, unexpected ) :: _ ->
            UnexpectedTokenForExpression i unexpected |> Error

        [] ->
            Error EndOfFileDuringExpression


prattLoop : Int -> Int -> Expression -> List Token -> Parser ExpressionParsingError Expression
prattLoop indent limit left afterLeft =
    case afterLeft of
        ( _, T_BinaryOperator operator ) :: afterOperator ->
            let
                { associativity, precedence } =
                    operatorInfo operator
            in
            if precedence > limit then
                let
                    finalPrecedence : Int
                    finalPrecedence =
                        if associativity == Right then
                            precedence - 1

                        else
                            precedence
                in
                case pratt indent finalPrecedence afterOperator of
                    Parsed right afterRight ->
                        let
                            newLeft : Expression
                            newLeft =
                                Binary operator left right
                        in
                        prattLoop indent limit newLeft afterRight

                    Error e ->
                        Error e

            else
                Parsed left afterLeft

        _ ->
            Parsed left afterLeft


type Associativity
    = Left
    | Right
    | None


operatorInfo : Operator -> { associativity : Associativity, precedence : Int }
operatorInfo operator =
    case operator of
        Add ->
            { associativity = Left, precedence = 7 }

        Divide ->
            { associativity = Left, precedence = 8 }

        -- EqualTo ->
        --     Info None 5 " == "
        Power ->
            { associativity = Right, precedence = 9 }

        -- GreaterThan ->
        --     Info None 5 " > "
        -- GreaterThanOrEqual ->
        --     Info None 5 " >= "
        -- LessThan ->
        --     Info None 5 " < "
        -- LessThanOrEqual ->
        --     Info None 5 " <= "
        -- NotEqualTo ->
        --     Info None 5 " != "
        Multiply ->
            { associativity = Left, precedence = 8 }

        Subtract ->
            { associativity = Left, precedence = 7 }



-- Checking import cycles
{-

   checkImportCycles : List File -> BackendTask FatalError (List File)
   checkImportCycles files =
       let
           dictionary : Dict comparable File
           dictionary =
               List.foldl (\file -> Dict.insert file.name file) Dict.empty files
       in
       case checkCycles "Main" (Set.singleton "main") "Main" dictionary [] of
           Nothing ->
               Debug.todo ""

           Just error ->
               Debug.todo "branch 'Just _' not implemented"


   type ImportCycleError
       = ModuleCycle String
       | ImportedModuleNotFound String String


   checkCycles : String -> Set String -> String -> Dict String File -> List String -> Maybe ImportCycleError
   checkCycles this functionsToCheck old modules visited =
       case Dict.get this modules of
           Just { imports, functions } ->
               if List.member this visited then
                   ModuleCycle visited |> Just

               else
                   case
                       Set.foldr
                           (\f acc ->
                               case acc of
                                   Just error ->
                                       Just error

                                   Nothing ->
                                       case Dict.get f functions of
                                           Just (UncheckedFunction _ Public _) ->
                                               Nothing

                                           Just (UncheckedFunction _ Private _) ->
                                               ImportingUnexposedFunction f this old
                                                   |> Just

                                           Just (ExposedFrom _) ->
                                               ImportingUndeclaredFunction f this old
                                                   |> Just

                                           Just (CheckedFunction _ _ _) ->
                                               InternalErrorFoundAlreadyTyped f
                                                   |> Just

                                           Nothing ->
                                               ImportingUndeclaredFunction f this old
                                                   |> Just
                           )
                           Nothing
                           functionsToCheck
                   of
                       Just error ->
                           Just error

                       Nothing ->
                           Dict.foldl
                               (\importName newFunctions acc ->
                                   case acc of
                                       Just error ->
                                           Just error

                                       Nothing ->
                                           checkCycles importName
                                               newFunctions
                                               this
                                               modules
                                               (this :: visited)
                               )
                               Nothing
                               imports

           Nothing ->
               ImportedModuleNotFound this old |> Just

-}
-- Rendering


type alias JsDeclaration =
    { name : String
    , expression : JsExpression
    }


type JsExpression
    = JsLiteral JsLiteral
    | JsBinary Operator JsExpression JsExpression


type JsLiteral
    = JsNumber Natural (Maybe Natural)
    | JsScientificNumber ScientificNumber Sign Natural


fileToJs : ParsedFile -> List JsDeclaration
fileToJs =
    .declarations >> List.map declarationToJs


declarationToJs : Declaration -> JsDeclaration
declarationToJs declaration =
    { name = declaration.name
    , expression = expressionToJs declaration.expression
    }


expressionToJs : Expression -> JsExpression
expressionToJs expression =
    case expression of
        Literal _ literal ->
            JsLiteral (literalToJs literal)

        Binary operator left right ->
            JsBinary operator (expressionToJs left) (expressionToJs right)


literalToJs : Literal -> JsLiteral
literalToJs literal =
    case literal of
        Number _ n ->
            JsNumber n Nothing

        Decimal before after ->
            JsNumber before (Just after)

        Scientific _ _ _ ->
            Debug.todo "branch 'Scientific _ _ _' not implemented"



-- Format


fileToString : ParsedFile -> String
fileToString { exports, imports, declarations } =
    let
        renderedDeclarations : String
        renderedDeclarations =
            List.map declarationToString declarations |> String.join "\n\n"
    in
    exportsToString exports ++ importsToString imports ++ renderedDeclarations


exportsToString : Exports -> String
exportsToString exports =
    if Set.isEmpty exports then
        ""

    else
        Set.foldl (\el acc -> acc ++ "\n    " ++ el) "export" exports
            ++ "\n\n"


importsToString : Imports -> String
importsToString imports =
    if Dict.isEmpty imports then
        ""

    else
        Dict.foldl
            (\moduleName exposedFunctions acc ->
                acc
                    ++ "\n    "
                    ++ moduleName
                    ++ Set.foldl
                        (\exposedFunction exposedAcc ->
                            exposedAcc
                                ++ "\n        "
                                ++ exposedFunction
                        )
                        ""
                        exposedFunctions
            )
            "import"
            imports
            ++ "\n\n"


declarationToString : Declaration -> String
declarationToString declaration =
    declaration.name ++ " = " ++ expressionToString declaration.expression


expressionToString : Expression -> String
expressionToString expression =
    case expression of
        Literal _ literal ->
            literalToString literal

        Binary operator a b ->
            {-
                Formatting with precedence rules

                Child precedence        Child precedence          Child precedence
               higher than parent:      lower than parent:        equal to parent:
                  no parenthesis          parenthesize           check associativity
                                                                     and branch

                  a * b + c                 (a + b) * c         a / b * c != a / (b * c)

                    add                         mul
                   /   \                       /   \
                 mul   c                     add    c
                /   \                       /   \
               a     b                     a     b

            -}
            let
                parent : { associativity : Associativity, precedence : Int }
                parent =
                    operatorInfo operator

                left : String
                left =
                    case a of
                        Binary op _ _ ->
                            let
                                leftChild :
                                    { associativity : Associativity
                                    , precedence : Int
                                    }
                                leftChild =
                                    operatorInfo op
                            in
                            if leftChild.precedence > parent.precedence then
                                expressionToString a

                            else if leftChild.precedence < parent.precedence then
                                "(" ++ expressionToString a ++ ")"

                            else if parent.associativity == Left then
                                expressionToString a

                            else
                                "(" ++ expressionToString a ++ ")"

                        -- Negate _ ->
                        --     case operator of
                        --         Caret ->
                        --             "(" ++ toString a ++ ")"
                        --         _ ->
                        --             toString a
                        _ ->
                            expressionToString a

                right : String
                right =
                    case b of
                        Binary op _ _ ->
                            let
                                rightChild :
                                    { associativity : Associativity
                                    , precedence : Int
                                    }
                                rightChild =
                                    operatorInfo op
                            in
                            if rightChild.precedence > parent.precedence then
                                expressionToString b

                            else if rightChild.precedence < parent.precedence then
                                "(" ++ expressionToString b ++ ")"

                            else if parent.associativity == Right then
                                expressionToString b

                            else
                                "(" ++ expressionToString b ++ ")"

                        _ ->
                            expressionToString b
            in
            left ++ " " ++ operatorToString operator ++ " " ++ right


literalToString : Literal -> String
literalToString literal =
    case literal of
        Number Base16 n ->
            "0x" ++ Natural.toHexString n

        Number Base10 n ->
            Natural.toDecimalString n

        Number Base8 n ->
            "0o" ++ Natural.toOctalString n

        Number Base2 n ->
            "0b" ++ Natural.toBinaryString n

        Decimal before after ->
            Natural.toString before ++ "." ++ Natural.toString after

        Scientific (IntegerBase n) Positive x ->
            Natural.toString n ++ "e" ++ Natural.toString x

        Scientific (IntegerBase n) Negative x ->
            Natural.toString n ++ "e-" ++ Natural.toString x

        Scientific (DecimalBase n m) Positive x ->
            Natural.toString n
                ++ "."
                ++ Natural.toString m
                ++ "e"
                ++ Natural.toString x

        Scientific (DecimalBase n m) Negative x ->
            Natural.toString n
                ++ "."
                ++ Natural.toString m
                ++ "e-"
                ++ Natural.toString x


jsFileToString : List JsDeclaration -> String
jsFileToString =
    List.map jsDeclarationToString >> String.join "\n\n"


jsDeclarationToString : JsDeclaration -> String
jsDeclarationToString declaration =
    "const "
        ++ declaration.name
        ++ " = \n    "
        ++ jsExpressionToString declaration.expression


jsExpressionToString : JsExpression -> String
jsExpressionToString expression =
    case expression of
        JsLiteral literal ->
            jsLiteralToString literal

        JsBinary _ _ _ ->
            Debug.todo "branch 'JsBinary _ _ _' not implemented"


jsLiteralToString : JsLiteral -> String
jsLiteralToString literal =
    case literal of
        JsNumber n Nothing ->
            Natural.toString n

        JsNumber before (Just after) ->
            Natural.toString before ++ "." ++ Natural.toString after

        JsScientificNumber (IntegerBase n) Positive x ->
            Natural.toString n ++ "e" ++ Natural.toString x

        JsScientificNumber (IntegerBase n) Negative x ->
            Natural.toString n ++ "e-" ++ Natural.toString x

        JsScientificNumber (DecimalBase n m) Positive x ->
            Natural.toString n
                ++ "."
                ++ Natural.toString m
                ++ "e"
                ++ Natural.toString x

        JsScientificNumber (DecimalBase n m) Negative x ->
            Natural.toString n
                ++ "."
                ++ Natural.toString m
                ++ "e-"
                ++ Natural.toString x



-- Error messages


errorToString : DeclarationParsingError -> String
errorToString error =
    case error of
        UnexpectedToken ( _, id ) ->
            "I’m trying to parse a declaration but I found " ++ describeToken id

        ExpressionParsingError _ str e ->
            "I’m trying to parse "
                ++ str
                ++ " but I have a problem. "
                ++ expressionParsingErrorToString e

        DeclarationMustBeLowercase ( _, id ) ->
            "Declarations must start with a lowercase word, but I found "
                ++ describeToken id

        ExpectingEqual ( _, id ) ->
            "I was expecting an equal symbol = after this declaration’s name, but I found "
                ++ describeToken id


expressionParsingErrorToString : ExpressionParsingError -> String
expressionParsingErrorToString error =
    case error of
        EndOfFileDuringExpression ->
            "The file ended."

        UnexpectedTokenForExpression _ token ->
            "I encountered " ++ describeToken token

        ExpectingIndentAtLeast { minIndent } ->
            "I was expecting some indentation, but there’s none"

        MissingClosingParenthesis _ ->
            "I was expecting a closing parenthesis, but I found none"


describeToken : Id -> String
describeToken id =
    case id of
        T_Literal literal ->
            describeLiteral literal

        T_Lowercase str ->
            "the word " ++ str

        T_Uppercase str ->
            "the word " ++ str

        T_Equal ->
            "the symbol ="

        T_Indent n ->
            "an indentation of " ++ String.fromInt n ++ " spaces"

        T_Illegal str ->
            "something weird: " ++ str

        T_Export ->
            "the export keyword"

        T_Import ->
            "the import keyword"

        T_BinaryOperator operator ->
            "the " ++ operatorToString operator ++ " operator"

        T_OpenParenthesis ->
            "an open parenthesis"

        T_CloseParenthesis ->
            "a closing parenthesis"


describeLiteral : Literal -> String
describeLiteral literal =
    case literal of
        Decimal _ _ ->
            "the decimal number " ++ literalToString literal

        Number Base16 _ ->
            "the hexadecimal number " ++ literalToString literal

        Number Base10 _ ->
            "the number " ++ literalToString literal

        Number Base8 _ ->
            "the octal number " ++ literalToString literal

        Number Base2 _ ->
            "the binary number " ++ literalToString literal

        Scientific _ _ _ ->
            "the number in scientific notation " ++ literalToString literal


scientificToString : ScientificNumber -> String
scientificToString number =
    case number of
        IntegerBase n ->
            Natural.toString n

        DecimalBase n m ->
            Natural.toString n ++ "." ++ Natural.toString m


operatorToString : Operator -> String
operatorToString operator =
    case operator of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"

        Power ->
            "^"
