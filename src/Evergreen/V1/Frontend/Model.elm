module Evergreen.V1.Frontend.Model exposing (..)

import Browser.Navigation
import Evergreen.V1.Logic.Initiative
import Evergreen.V1.Types.Actions
import Evergreen.V1.Types.Levels
import Evergreen.V1.Types.Player
import Evergreen.V1.Types.Weapons
import Random
import Url


type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , random_seed : Random.Seed
    , player : Maybe Evergreen.V1.Types.Player.Player
    , current_weapon : Evergreen.V1.Types.Weapons.Weapon
    , base_weapon : Evergreen.V1.Types.Weapons.Weapon
    , temp_player : Maybe Evergreen.V1.Types.Player.Player
    , player_status : Evergreen.V1.Types.Player.Status
    , game_started : Bool
    , class_picked : Bool
    , point_buy_complete : Bool
    , points_to_spend : Int
    , room_entry_type : Evergreen.V1.Types.Levels.RoomEntryType
    , current_room : Int
    , current_room_enemy_num : Int
    , round_turn_list : List Evergreen.V1.Logic.Initiative.CharacterWithInitiative
    , selected_enemy_id : Int
    , show_player_action_options : Bool
    , player_stealthed : ( Bool, Int )
    , player_stealth_cooldown : Int
    , enemy_taunted : ( Bool, Int )
    , player_taunt_cooldown : Int
    , furious_attack_cooldown : Int
    , self_heal_cooldown : Int
    , distance_from_enemy : Evergreen.V1.Types.Actions.Distance
    , current_level : Evergreen.V1.Types.Levels.Level
    , show_help_menu : Bool
    , event_log : List String
    }
