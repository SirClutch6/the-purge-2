module Evergreen.V1.Logic.Initiative exposing (..)

import Evergreen.V1.Types.Enemy
import Evergreen.V1.Types.Player


type Character
    = Player Evergreen.V1.Types.Player.Player
    | Enemy Evergreen.V1.Types.Enemy.Enemy


type alias CharacterWithInitiative =
    ( Character, Int )
