module Types.Enemy exposing (..)

type Class
    = Rogue
    | Spy
    | Warrior
    | Tank
    | Boss
    | Captain

type alias Enemy =
    { class : Class
    , hp : Int
    , max_hp : Int
    , dexterity : Int
    , strength : Int
    , charisma : Int
    , constitution : Int
    , level : Int
    , rush : Int
    }

-- Get level from model

baseEnemyRogue : Int -> Enemy
baseEnemyRogue level =
    { class = Rogue
    , hp = 0
    , max_hp = 0
    , dexterity = 2 + level^2
    , strength = 0 + level^2
    , charisma = 0 + level
    , constitution = -1 + (getLevelConversionInt level)
    , level = level
    , rush = 0
    }

baseEnemySpy : Int -> Enemy
baseEnemySpy level =
    { class = Spy
    , hp = 0
    , max_hp = 0
    , dexterity = 0 + level^2
    , strength = -1 + (getLevelConversionInt level)
    , charisma = 2 + level^2
    , constitution = 0 + level
    , level = level
    , rush = 0
    }

baseEnemyWarrior : Int -> Enemy
baseEnemyWarrior level =
    { class = Warrior
    , hp = 0
    , max_hp = 0
    , dexterity = 1 + level^2
    , strength = 1 + level^2
    , charisma = -1 + level
    , constitution = 0 + (getLevelConversionInt level)
    , level = level
    , rush = 0
    }

baseEnemyTank : Int -> Enemy
baseEnemyTank level =
    { class = Tank
    , hp = 0
    , max_hp = 0
    , dexterity = -1 + level
    , strength = 0 + level^2
    , charisma = 0 + (getLevelConversionInt level)
    , constitution = 2 + level^2
    , level = level
    , rush = 0
    }

baseEnemyBoss : Int -> Enemy
baseEnemyBoss level =
    { class = Boss
    , hp = 0
    , max_hp = 0
    , dexterity = 1 + level^2
    , strength = 1 + level^2
    , charisma = 1 + level
    , constitution = 1 + level^2
    , level = level
    , rush = 0
    }

baseEnemyCaptain : Int -> Enemy
baseEnemyCaptain level =
    { class = Captain
    , hp = 0
    , max_hp = 0
    , dexterity = 0 --TODO Random numbers between 8 and 15
    , strength = 0
    , charisma = 0
    , constitution = 0
    , level = level
    , rush = 0
    }

-- q: how do i get a random number in elm?
-- a: https://package.elm-lang.org/packages/elm/random/latest/Random

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

adjustRush : Int -> Enemy -> Enemy
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