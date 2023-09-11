module Frontend.Model exposing (..)

import Browser.Navigation as Nav
import Url

import Types.Actions as Actions
import Types.Enemy as Enemy
import Types.Levels as Levels
import Types.Player as Player
import Types.VendingItems as VI
import Types.Weapons as Weapon

type alias Model =
    { key : Nav.Key
    , url : Url.Url

    }

type DefineModel
    = Init ModelNonResetableFields
    | Reset Model

type alias ModelNonResetableFields =
    { key : Nav.Key
    , url : Url.Url
    -- , scene_context : XCSN.SceneContext Scene.Scene
    }

defineModel : DefineModel -> Model
defineModel define_model =
    let
        non_resetable_fields =
            case define_model of
                Init data ->
                    data

                Reset model ->
                    ModelNonResetableFields
                        model.key
                        model.url
                        -- model.scene_context
    in
    { key = non_resetable_fields.key
    , url = non_resetable_fields.url
    }