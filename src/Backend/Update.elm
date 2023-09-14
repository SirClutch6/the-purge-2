module Backend.Update exposing (..)

import Types

update : Types.BackendMsg -> Types.BackendModel -> (Types.BackendModel, Cmd Types.BackendMsg)
update _ model =
    ( model, Cmd.none)