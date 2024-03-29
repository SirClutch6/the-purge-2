module Types.Actions exposing (..)

import Types.Weapons as W
import Types.Player as P
import Types.Enemy as E
import Logic.RandomGen as RNG

import Random


type Distance
    = Melee
    | Range

type Direction
    = Toward
    | Away

type PlayerAction
    = RangedAttack
    | MeleeAttack
    | Move Direction
    | Taunt
    | FuriousAttack
    | Stealth
    | Heal

type EnemyAction
    = EnemyRangedAttack
    | EnemyMeleeAttack
    | EnemyMove Direction
    | EnemyTaunt

type AfterRoom
    = Rest
    | Loot
    | Rush

distanceToString : Distance -> String
distanceToString distance =
    case distance of
        Melee ->
            "Melee"

        Range ->
            "Range"

directionToString : Direction -> String
directionToString direction =
    case direction of
        Toward ->
            "Toward"

        Away ->
            "Away"

playerActionToString : PlayerAction -> String
playerActionToString action =
    case action of
        RangedAttack ->
            "Ranged Attack"

        MeleeAttack ->
            "Melee Attack"

        Move direction ->
            "Move " ++ directionToString direction

        Taunt ->
            "Taunt"

        FuriousAttack ->
            "Furious Attack"

        Stealth ->
            "Stealth"

        Heal ->
            "Heal"

enemyActionToString : EnemyAction -> String
enemyActionToString action =
    case action of
        EnemyRangedAttack ->
            "Ranged Attack"

        EnemyMeleeAttack ->
            "Melee Attack"

        EnemyMove direction ->
            "Move " ++ directionToString direction

        EnemyTaunt ->
            "Taunt"

enemyRogueActionOptions : Distance -> List EnemyAction
enemyRogueActionOptions distance =
    case distance of
        Melee ->
            [ EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyTaunt
            ]

        Range ->
            [ EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyTaunt
            ]

enemySpyActionOptions : Distance -> List EnemyAction
enemySpyActionOptions distance =
    case distance of
        Melee ->
            [ EnemyMove Away
            , EnemyMove Away
            , EnemyMove Away
            , EnemyMove Away
            , EnemyMove Away
            , EnemyMove Away
            , EnemyMove Away
            , EnemyTaunt
            , EnemyTaunt
            , EnemyTaunt
            ]

        Range ->
            [ EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyTaunt
            , EnemyTaunt
            , EnemyTaunt
            , EnemyTaunt
            ]

enemyWarriorActionOptions : Distance -> List EnemyAction
enemyWarriorActionOptions distance =
    case distance of
        Melee ->
            [ EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMove Away
            ]

        Range ->
            [ EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            ]

enemyTankActionOptions : Distance -> List EnemyAction
enemyTankActionOptions distance =
    case distance of
        Melee ->
            [ EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            ]

        Range ->
            [ EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyMove Toward
            , EnemyTaunt
            , EnemyTaunt
            ]

enemyBossActionOptions : Distance -> List EnemyAction
enemyBossActionOptions distance =
    case distance of -- TODO depends on random weapon
        Melee ->
            [ EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyTaunt
            , EnemyTaunt
            ]

        Range ->
            [ EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyTaunt
            , EnemyTaunt
            ]

enemyCaptainActionOptions : Distance -> List EnemyAction
enemyCaptainActionOptions distance =
    case distance of
        Melee ->
            [ EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyMeleeAttack
            , EnemyTaunt
            , EnemyTaunt
            ]

        Range ->
            [ EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyRangedAttack
            , EnemyTaunt
            , EnemyTaunt
            ]

getEnemyActions : E.Enemy -> Distance -> (List EnemyAction, W.Weapon)
getEnemyActions enemy distance =
    case enemy.class of
        E.Rogue ->
            (enemyRogueActionOptions distance, W.Taser)

        E.Spy ->
            (enemySpyActionOptions distance, W.Pistol)

        E.Warrior ->
            (enemyWarriorActionOptions distance, W.MachineGun)

        E.Tank ->
            (enemyTankActionOptions distance, W.NightStick)

        E.Boss ->

            (enemyBossActionOptions distance, W.MachineGun)

        E.Captain ->
            if distance == Range then
                (enemyCaptainActionOptions distance, W.MachineGun)
            else 
                (enemyCaptainActionOptions distance, W.NightStick)

afterRoomEffect : AfterRoom -> Int -> Random.Seed -> P.Player -> (P.Player, Random.Seed)
afterRoomEffect effect num_enemies seed player =
    case effect of
        Rest ->
            (P.adjustHealth 5 player |> P.adjustSanity 5, seed)

        Loot ->
            let
                (coins, new_seed) =
                    -- List.length enemy_list * 2 --TODO random number of coins
                    -- List.foldl getRandomCoins ([], seed) enemy_list 
                    Random.step (RNG.randomListGen num_enemies RNG.oneToFive) seed
                    
                
                new_coins = List.sum coins
            in
            (P.adjustCoins new_coins player, new_seed)
            -- TODO take new weapon

        Rush ->
            (P.adjustRush 1 player, seed)

-- getRandomCoins : E.Enemy -> (List Int, Random.Seed) -> (List Int, Random.Seed)
-- getRandomCoins _ (coins, seed) =
--     let
--         (new_coins, new_seed) =
--             Random.step (Random.int 1 5) seed
--     in
--     (new_coins :: coins, new_seed)