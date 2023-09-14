module Types.Player exposing (..)

type Status
    = NotStarted
    | StartedEntry
    | InRoom
    | BetweenRooms
    | BetweenLevels
    | Finished
    | Lost

type Class
    = Rogue
    | Spy
    | Warrior
    | Tank

type Attribute
    = Dexterity
    | Strength
    | Charisma
    | Constitution

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
    -- , perks : List Perk
    }

classToString : Class -> String
classToString class =
    case class of
        Rogue ->
            "Rogue"

        Spy ->
            "Spy"

        Warrior ->
            "Warrior"

        Tank ->
            "Tank"

defaultPlayer : Player
defaultPlayer =
    { class = Rogue
    , hp = 0
    , max_hp = 0
    , dexterity = 0
    , strength = 0
    , charisma = 0
    , constitution = 0
    , sanity = 0
    , max_sanity = 0
    , rush = 0
    , coins = 0
    , turn_initiative = 0
    }

baseRogue : Player
baseRogue =
    { class = Rogue
    , hp = 0
    , max_hp = 0
    , dexterity = 3
    , strength = 1
    , charisma = 0
    , constitution = -1
    , sanity = 100
    , max_sanity = 100
    , rush = 0
    , coins = 0
    , turn_initiative = 0
    }

baseSpy : Player
baseSpy =
    { class = Spy
    , hp = 0
    , max_hp = 0
    , dexterity = 1
    , strength = -1
    , charisma = 3
    , constitution = 0
    , sanity = 100
    , max_sanity = 100
    , rush = 0
    , coins = 0
    , turn_initiative = 0
    }

baseWarrior : Player
baseWarrior =
    { class = Warrior
    , hp = 0
    , max_hp = 0
    , dexterity = 1
    , strength = 2
    , charisma = -1
    , constitution = 1
    , sanity = 100
    , max_sanity = 100
    , rush = 0
    , coins = 0
    , turn_initiative = 0
    }

baseTank : Player
baseTank =
    { class = Tank
    , hp = 0
    , max_hp = 0
    , dexterity = -1
    , strength = 1
    , charisma = 0
    , constitution = 3
    , sanity = 100
    , max_sanity = 100
    , rush = 0
    , coins = 0
    , turn_initiative = 0
    }

calculateHP : Player -> Player
calculateHP player =
    let
        base_hp = 10
        new_hp =
            case player.class of
                Rogue ->
                    base_hp + (toFloat player.constitution) * 1.0

                Spy ->
                    base_hp + (toFloat player.constitution) * 1.2

                Warrior ->
                    base_hp + (toFloat player.constitution) * 1.5

                Tank ->
                    base_hp + (toFloat player.constitution) * 2.0
    in
    { player | hp = round new_hp, max_hp = round new_hp }

adjustHealth : Int -> Player -> Player
adjustHealth amount player =
    let
        new_hp =
            if player.hp + amount > player.max_hp then
                player.max_hp
            else if player.hp + amount < 0 then
                0
            else
                player.hp + amount
    in
    { player | hp = new_hp }

adjustSanity : Int -> Player -> Player
adjustSanity amount player =
    let
        new_sanity =
            if player.sanity + amount > player.max_sanity then
                player.max_sanity
            else if player.sanity + amount < 0 then
                0
            else
                player.sanity + amount
    in
    { player | sanity = new_sanity }

adjustMaxHealth : Int -> Player -> Player
adjustMaxHealth amount player =
    let
        new_max_hp =
            if player.max_hp + amount < 0 then
                0
            else
                player.max_hp + amount
    in
    { player | max_hp = new_max_hp, hp = player.hp + amount }

adjustRush : Int -> Player -> Player
adjustRush amount player =
    let
        new_rush =
            if player.rush + amount < 0 then
                0
            else if player.rush + amount > 2 then
                2
            else
                player.rush + amount
    in
    { player | rush = new_rush }

adjustCoins : Int -> Player -> Player
adjustCoins amount player =
    let
        new_coins =
            if player.coins + amount < 0 then
                0
            else
                player.coins + amount
    in
    { player | coins = new_coins }

adjustInitiative : Player -> Int -> Player
adjustInitiative player inv =
    { player | turn_initiative = inv }

getStat : String -> Player -> Int
getStat stat player =
    case stat of
        "HP" ->
            player.hp

        "Max HP" ->
            player.max_hp

        "Dexterity" ->
            player.dexterity

        "Strength" ->
            player.strength

        "Charisma" ->
            player.charisma

        "Constitution" ->
            player.constitution

        "Sanity" ->
            player.sanity

        "Max Sanity" ->
            player.max_sanity

        "Rush" ->
            player.rush

        "Coins" ->
            player.coins

        "Turn Initiative" ->
            player.turn_initiative

        _ ->
            0