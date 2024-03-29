module Types.Enemy exposing (..)

type Class
    = Rogue
    | Spy
    | Warrior
    | Tank
    | Boss
    | Captain

-- type EnemyDistance
--     = Melee
--     | Range

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
    -- , distance_from_player : EnemyDistance
    }

classToString : Class -> String
classToString enemy_class =
    case enemy_class of
        Rogue ->
            "Rogue"
        Spy ->
            "Spy"
        Warrior ->
            "Warrior"
        Tank ->
            "Tank"
        Boss ->
            "Boss"
        Captain ->
            "Captain"

-- Get level from model

defaultEnemy : Enemy
defaultEnemy =
    { class = Rogue
    , id = 0
    , hp = 0
    , max_hp = 0
    , dexterity = 0
    , strength = 0
    , charisma = 0
    , constitution = 0
    , level = 0
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

baseEnemyRogue : Int -> Int -> Enemy
baseEnemyRogue level id =
    { class = Rogue
    , id = id
    , hp = 0
    , max_hp = 0
    , dexterity = 2 + level^2
    , strength = 0 + level^2
    , charisma = 0 + level
    , constitution = -1 + (getLevelConversionInt level)
    , level = level
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

baseEnemySpy : Int -> Int -> Enemy
baseEnemySpy level id =
    { class = Spy
    , id = id
    , hp = 0
    , max_hp = 0
    , dexterity = 0 + level^2
    , strength = -1 + (getLevelConversionInt level)
    , charisma = 2 + level^2
    , constitution = 0 + level
    , level = level
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

baseEnemyWarrior : Int -> Int -> Enemy
baseEnemyWarrior level id =
    { class = Warrior
    , id = id
    , hp = 0
    , max_hp = 0
    , dexterity = 1 + level^2
    , strength = 1 + level^2
    , charisma = -1 + level
    , constitution = 0 + (getLevelConversionInt level)
    , level = level
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

baseEnemyTank : Int -> Int -> Enemy
baseEnemyTank level id =
    { class = Tank
    , id = id
    , hp = 0
    , max_hp = 0
    , dexterity = -1 + level
    , strength = 0 + level^2
    , charisma = 0 + (getLevelConversionInt level)
    , constitution = 2 + level^2
    , level = level
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

baseEnemyBoss : Int -> Int -> Enemy
baseEnemyBoss level id =
    { class = Boss
    , id = id
    , hp = 0
    , max_hp = 0
    , dexterity = 1 + level^2
    , strength = 1 + level^2
    , charisma = 1 + level
    , constitution = 1 + level^2
    , level = level
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

baseEnemyCaptain : Int -> Int -> Enemy
baseEnemyCaptain level id =
    { class = Captain
    , id = id
    , hp = 0
    , max_hp = 0
    , dexterity = 0 --TODO Random numbers between 8 and 15
    , strength = 0
    , charisma = 0
    , constitution = 0
    , level = level
    , rush = 0
    , turn_initiative = 0
    -- , distance_from_player = Range
    }

getCaptainConstitution : Enemy -> Enemy
getCaptainConstitution captain =
    let
        new_const = 
            60 - captain.dexterity - captain.strength - captain.charisma
    in
    { captain | constitution = new_const }

calculateEnemyHP : Enemy -> Enemy
calculateEnemyHP enemy =
    let
        base_hp = 5
        new_hp =
            case enemy.class of
                Rogue ->
                    base_hp + (toFloat enemy.constitution * (2.0 + (getConstConversionInt enemy.level 10)))

                Spy ->
                    base_hp + (toFloat enemy.constitution * (1.2 + (getConstConversionInt enemy.level 10)))

                Warrior ->
                    base_hp + (toFloat enemy.constitution * (1.5 + (getConstConversionInt enemy.level 10)))

                Tank ->
                    base_hp + (toFloat enemy.constitution * (2.0 + (getConstConversionInt enemy.level 10)))

                Boss ->
                    base_hp + (toFloat enemy.constitution * (2.0 + (getConstConversionInt enemy.level 10)))

                Captain ->
                    40 + (toFloat enemy.constitution)
    in
    { enemy | hp = (round new_hp), max_hp = (round new_hp) }

getLevelConversionInt : Int -> Int
getLevelConversionInt level =
    let
        square = toFloat level^2
        division = square / 2
    in
    round division

getConstConversionInt : Int -> Int -> Float
getConstConversionInt level denominator =
    let
        square = toFloat level^2
        division = square / toFloat denominator
    in
    division

adjustRush : Int -> Enemy -> Enemy --TODO rework this so anyone can have rush?
adjustRush amount enemy =
    let
        new_rush = 
            if enemy.rush + amount < 0 then
                0
            else
                enemy.rush + amount
    in
    case enemy.class of
        Rogue ->
            let
                rush_new = 
                    if new_rush > 1 then
                        1
                    else
                        new_rush
            in
            { enemy | rush = rush_new }

        Boss ->
            { enemy | rush = new_rush }

        Captain ->
            { enemy | rush = new_rush }

        _ ->
            enemy

adjustHealth : Int -> Enemy -> Enemy
adjustHealth amount enemy =
    let
        new_hp = 
            if enemy.hp + amount < 0 then
                0
            else if enemy.hp + amount > enemy.max_hp then
                enemy.max_hp
            else
                enemy.hp + amount
    in
    { enemy | hp = new_hp }

adjustInitiative : Enemy -> Int -> Enemy
adjustInitiative enemy inv =
    { enemy | turn_initiative = inv }

getStat : String -> Enemy -> Int
getStat stat enemy =
    case stat of
        "HP" ->
            enemy.hp
        "Max HP" ->
            enemy.max_hp
        "Dexterity" ->
            enemy.dexterity
        "Strength" ->
            enemy.strength
        "Charisma" ->
            enemy.charisma
        "Constitution" ->
            enemy.constitution
        "Level" ->
            enemy.level
        "Rush" ->
            enemy.rush
        "Turn Initiative" ->
            enemy.turn_initiative
        _ ->
            0