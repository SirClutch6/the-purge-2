module Evergreen.V1.Types.Enemy exposing (..)


type Class
    = Rogue
    | Spy
    | Warrior
    | Tank
    | Boss
    | Captain


type alias Enemy =
    { class : Class
    , id : Int
    , hp : Int
    , max_hp : Int
    , dexterity : Int
    , strength : Int
    , charisma : Int
    , constitution : Int
    , level : Int
    , rush : Int
    , turn_initiative : Int
    }
