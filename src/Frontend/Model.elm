module Frontend.Model exposing (..)

import Browser.Navigation as Nav
import Url

import Types.Actions as Actions
import Types.Enemy as Enemy
import Types.Levels as Levels
import Types.Player as Player
import Types.VendingItems as VI
import Types.Weapons as Weapon
import Logic.Initiative as Inv

import Random
-- import Time
-- import Task

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , random_seed : Random.Seed
    , player : Maybe Player.Player
    , current_weapon : Weapon.Weapon
    , base_weapon : Weapon.Weapon
    , temp_player : Maybe Player.Player
    , player_status : Player.Status 
    , game_started : Bool
    , class_picked : Bool
    , point_buy_complete : Bool
    , points_to_spend : Int
    , room_entry_type : Levels.RoomEntryType
    , current_room : Int
    , current_room_enemy_num : Int
    , round_turn_list : List Inv.CharacterWithInitiative
    , selected_enemy_id : Int
    -- , selected_enemy_distance : Actions.Distance
    , show_player_action_options : Bool
    , player_stealthed : (Bool, Int)
    , player_stealth_cooldown : Int
    , enemy_taunted : (Bool, Int)
    , player_taunt_cooldown : Int
    , furious_attack_cooldown : Int
    , self_heal_cooldown : Int
    , distance_from_enemy : Actions.Distance
    , current_level : Levels.Level
    , show_help_menu : Bool
    , event_log : List String
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
    , random_seed = Random.initialSeed 0
    , player = Nothing
    , current_weapon = Weapon.Other "Nothing"
    , base_weapon = Weapon.Other "Nothing"
    , temp_player = Nothing
    , player_status = Player.NotStarted
    , game_started = False
    , class_picked = False
    , point_buy_complete = False
    , points_to_spend = startingPoints
    , room_entry_type = Levels.Normal
    , current_room = 0
    , current_room_enemy_num = 0
    , round_turn_list = []
    , selected_enemy_id = -1
    -- , selected_enemy_distance = Actions.Range
    , show_player_action_options = False
    , player_stealthed = (False, 0)
    , player_stealth_cooldown = 0
    , enemy_taunted = (False, 0)
    , player_taunt_cooldown = 0
    , furious_attack_cooldown = 0
    , self_heal_cooldown = 0
    , distance_from_enemy = Actions.Range
    , current_level = Levels.level1
    , show_help_menu = False
    , event_log = []
    }

-- getTimeInt : Int
-- getTimeInt =
--     Time.posixToMillis (Task.perform Time.now)

-- cleanTime : Time.Posix -> Cmd msg
-- cleanTime time =
--     let
--         millis = Time.posixToMillis time
--     in
--     Cmd.none


startingPoints : Int
startingPoints =
    10

levelUpPoints : Int
levelUpPoints =
    3