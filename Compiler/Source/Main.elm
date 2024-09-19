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
        |> BackendTask.andThen Script.log
