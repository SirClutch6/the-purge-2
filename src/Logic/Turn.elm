module Logic.Turn exposing (..)

import Logic.Initiative as Inv
-- import Random

type Completed 
    = True
    | False

takeCharacterTurn : Inv.CharacterWithInitiative -> Completed
takeCharacterTurn character =
    True -- TODO implement this