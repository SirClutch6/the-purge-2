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
    , dexterity : Int
    , strength : Int
    , charisma : Int
    , constitution : Int
    , level : Int
    }

-- Get level from model

baseEnemyRogue : Int -> Enemy
baseEnemyRogue level =
    { class = Rogue
    , hp = 0
    , dexterity = 2 + level^2
    , strength = 0 + level^2
    , charisma = 0 + level
    , constitution = -1 + (level^2 / 2)
    , level = level
    }

baseEnemySpy : Int -> Enemy
baseEnemySpy level =
    { class = Spy
    , hp = 0
    , dexterity = 0 + level^2
    , strength = -1 + (level^2 / 2)
    , charisma = 2 + level^2
    , constitution = 0 + level
    , level = level
    }

baseEnemyWarrior : Int -> Enemy
baseEnemyWarrior level =
    { class = Warrior
    , hp = 0
    , dexterity = 1 + level^2
    , strength = 1 + level^2
    , charisma = -1 + level
    , constitution = 0 + (level^2 / 2)
    , level = level
    }

baseEnemyTank : Int -> Enemy
baseEnemyTank level =
    { class = Tank
    , hp = 0
    , dexterity = -1 + level
    , strength = 0 + level^2
    , charisma = 0 + (level^2 / 2)
    , constitution = 2 + level^2
    , level = level
    }

baseEnemyBoss : Int -> Enemy
baseEnemyBoss level =
    { class = Boss
    , hp = 0
    , dexterity = 1 + level^2
    , strength = 1 + level^2
    , charisma = 1 + level
    , constitution = 1 + level^2
    , level = level
    }

baseEnemyCaptain : Int -> Enemy
baseEnemyCaptain level =
    { class = Captain
    , hp = 0
    , dexterity = 0 --TODO Random numbers between 8 and 15
    , strength = 0
    , charisma = 0
    , constitution = 0
    , level = level
    }

-- q: how do i get a random number in elm?
-- a: https://package.elm-lang.org/packages/elm/random/latest/Random

getCaptainConstitution : Enemy -> Enemy
getCaptainConstitution captain =
    let
        new_const = 
            60 - captin.dexterity - captain.strength - captain.charisma
    in
    { captain | constitution = new_const }

calculateEnemyHP : Enemy -> Enemy
calculateEnemyHP enemy =
    let
        base_hp = 5
        new_hp =
            case enemy.class of
                Rogue ->
                    base_hp + player.constitution * (1.0 + player.level^2 / 10)

                Spy ->
                    base_hp + player.constitution * (1.2 + player.level^2 / 10)

                Warrior ->
                    base_hp + player.constitution * (1.5 + player.level^2 / 10)

                Tank ->
                    base_hp + player.constitution * (2.0 + player.level^2 / 10)

                Boss ->
                    base_hp + player.constitution * (2.0 + player.level^2 / 10)

                Captain ->
                    40 + enemy.constitution
    in
    { player | hp = (round new_hp) }


