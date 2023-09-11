module Frontend.Update exposing (update)

import Types

update : Types.FrontendMsg -> Types.FrontendModel -> (Types.FrontendModel, Cmd Types.FrontendMsg)
update msg model =
    ( model
    , Cmd.none
    )