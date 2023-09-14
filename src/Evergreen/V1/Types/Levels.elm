module Evergreen.V1.Types.Levels exposing (..)

import Evergreen.V1.Types.Enemy


type RoomEntryType
    = DexSneak
    | ChrSneak
    | FailedSneak
    | Rush
    | Normal


type Item
    = Stapler
    | StandingFan
    | Chair
    | Mug


type alias Room =
    { num : Int
    , items : List Item
    , enemies : List Evergreen.V1.Types.Enemy.Enemy
    }


type alias Level =
    { level : Int
    , image : String
    , rooms : List Room
    }
