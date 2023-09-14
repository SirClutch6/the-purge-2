module Evergreen.V1.Types.Player exposing (..)


type Class
    = Rogue
    | Spy
    | Warrior
    | Tank


type alias Player =
    { class : Class
    , hp : Int
    , max_hp : Int
    , dexterity : Int
    , strength : Int
    , charisma : Int
    , constitution : Int
    , sanity : Int
    , max_sanity : Int
    , rush : Int
    , coins : Int
    , turn_initiative : Int
    }


type Status
    = NotStarted
    | StartedEntry
    | InRoom
    | BetweenRooms
    | BetweenLevels
    | Finished
    | Lost


type Attribute
    = Dexterity
    | Strength
    | Charisma
    | Constitution
