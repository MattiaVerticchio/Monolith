module Main exposing (Id(..), Token, run, tokenize)

import BackendTask as Task exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.File as File
import BackendTask.Stream exposing (Error)
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
        |> parse
        |> check for duplicated functions
        |> check for cyclic imports
        |> topological sort
        |> type checking
        |> conversion
        |> emit files
-}


run : Script
run =
    Script.withoutCliOptions compile


compile : BackendTask FatalError ()
compile =
    readFiles path
        |> Task.map filterFiles
        |> Task.andThen readAndParseFiles
        |> Task.andThen (Debug.toString >> Script.log)


extension : String
extension =
    ".🗿"


path : String
path =
    "src"


filterFiles : List Dirent -> List String
filterFiles =
    List.filterMap <|
        \dirent ->
            if String.endsWith extension dirent.name then
                dirent.parentPath ++ "/" ++ dirent.name |> Just

            else
                Nothing


type alias Dirent =
    { name : String
    , parentPath : String
    }


readFiles : String -> BackendTask FatalError (List Dirent)
readFiles folder =
    Custom.run "readFolder" (Encode.string folder) (Decode.list decodeDirent)
        |> Task.allowFatal


decodeDirent : Decoder Dirent
decodeDirent =
    Decode.map2 Dirent
        (Decode.field "name" Decode.string)
        (Decode.field "parentPath" Decode.string)


readAndParseFiles : List String -> BackendTask FatalError (List { name : String, exports : Exports, imports : Imports, declarations : Dict String Declaration })
readAndParseFiles files =
    List.map (readAndParseFile >> Task.andThen checkDuplicatedDeclarations) files
        |> Task.combine


readAndParseFile : String -> BackendTask FatalError File
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
    = T_Number Natural
    | T_Lowercase String
    | T_Uppercase String
    | T_Equal
    | T_Export
    | T_Import
    | T_Indent Int
    | T_Illegal String


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

                afterNewline : Int
                afterNewline =
                    i + 1

                token : Token
                token =
                    ( afterNewline, T_Indent indent )

                afterSpace : Int
                afterSpace =
                    afterNewline + indent
            in
            t (token :: acc) afterSpace src afterIndent

        ' ' :: xs ->
            t acc (i + 1) src xs

        '=' :: xs ->
            t (( i, T_Equal ) :: acc) (i + 1) src xs

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
                case integer (i + 1) xs of
                    Continue newI rest ->
                        let
                            value : Natural
                            value =
                                String.slice i newI src
                                    |> Natural.fromSafeString

                            newAcc : List Token
                            newAcc =
                                ( i, T_Number value ) :: acc
                        in
                        t newAcc newI src rest

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


integer : Int -> List Char -> Tokenizer Int Int
integer length chars =
    case chars of
        [] ->
            Continue length chars

        x :: xs ->
            let
                code : Int
                code =
                    Char.toCode x
            in
            if isDigit code then
                integer (length + 1) xs

            else if isBreaking x then
                Continue length chars

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


type alias File =
    { name : String
    , exports : Exports
    , imports : Imports
    , declarations :
        -- The list is reversed
        List Declaration
    }


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
    = Number Int Natural


type ExpressionParsingError
    = UnexpectedTokenForExpression Int Id
    | EndOfFileDuringExpression


parseFile : String -> List Token -> BackendTask FatalError File
parseFile name tokens =
    case parseExports tokens of
        Error e ->
            exportParsingErrorToString e |> FatalError.fromString |> Task.fail

        Parsed exports afterExports ->
            case parseImports afterExports of
                Error e ->
                    Debug.toString e |> FatalError.fromString |> Task.fail

                Parsed imports afterImports ->
                    case parseDeclarations afterImports of
                        Parsed declarations _ ->
                            File name exports imports declarations
                                |> Task.succeed

                        Error e ->
                            errorToString e
                                |> FatalError.fromString
                                |> Task.fail


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
            case parseExpression afterEqual of
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


parseExpression : List Token -> Parser ExpressionParsingError Expression
parseExpression tokens =
    case tokens of
        ( i, T_Number n ) :: rest ->
            Parsed (Number i n) rest

        ( i, unexpected ) :: _ ->
            UnexpectedTokenForExpression i unexpected |> Error

        [] ->
            Error EndOfFileDuringExpression



-- Check for duplicated declarations


checkDuplicatedDeclarations :
    { name : String
    , exports : Exports
    , imports : Imports
    , declarations : List Declaration
    }
    ->
        BackendTask
            FatalError
            { name : String
            , exports : Exports
            , imports : Imports
            , declarations : Dict String Declaration
            }
checkDuplicatedDeclarations file =
    case checkDuplicatedDeclarationsHelp Dict.empty Dict.empty file.declarations of
        Ok declarations ->
            Task.succeed
                { name = file.name
                , exports = file.exports
                , imports = file.imports
                , declarations = declarations
                }

        Err duplicated ->
            "Duplicated functions: "
                ++ Debug.toString duplicated
                |> FatalError.fromString
                |> Task.fail


checkDuplicatedDeclarationsHelp :
    Dict String Declaration
    -> Dict String Int
    -> List Declaration
    -> Result (Dict String Int) (Dict String Declaration)
checkDuplicatedDeclarationsHelp acc duplicated remaining =
    case remaining of
        [] ->
            if Dict.isEmpty duplicated then
                Ok acc

            else
                Err duplicated

        x :: xs ->
            case Dict.get x.name acc of
                Nothing ->
                    checkDuplicatedDeclarationsHelp (Dict.insert x.name x acc)
                        duplicated
                        xs

                Just _ ->
                    let
                        counter : Int
                        counter =
                            case Dict.get x.name duplicated of
                                Nothing ->
                                    2

                                Just n ->
                                    n + 1
                    in
                    checkDuplicatedDeclarationsHelp acc
                        (Dict.insert x.name counter duplicated)
                        xs



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
    = JsNumber Natural


fileToJs : File -> List JsDeclaration
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
        Number _ n ->
            JsNumber n



-- Format


fileToString : File -> String
fileToString file =
    List.map declarationToString file.declarations |> String.join "\n\n"


declarationToString : Declaration -> String
declarationToString declaration =
    declaration.name ++ " = " ++ expressionToString declaration.expression


expressionToString : Expression -> String
expressionToString expression =
    case expression of
        Number _ n ->
            Natural.toString n


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
        JsNumber n ->
            Natural.toString n



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


describeToken : Id -> String
describeToken id =
    case id of
        T_Number n ->
            "the number " ++ Natural.toString n

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
