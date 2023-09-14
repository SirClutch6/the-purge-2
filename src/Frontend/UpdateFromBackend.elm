module Frontend.UpdateFromBackend exposing (..)

import Types

updateFromBackend : Types.ToFrontend -> Types.FrontendModel -> (Types.FrontendModel, Cmd Types.FrontendMsg)
updateFromBackend _ model =
    ( model, Cmd.none)