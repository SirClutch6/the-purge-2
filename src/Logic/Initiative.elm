module Logic.Initiative exposing (..)

import Types.Player as Player
import Types.Enemy as Enemy
import Random
import Logic.RandomGen as RNG
import List exposing (sort)

type Character
    = Player Player.Player
    | Enemy Enemy.Enemy

type alias CharacterWithInitiative =
    (Character, Int)

rollInitiative : Player.Player -> List Enemy.Enemy -> Random.Seed -> (Player.Player, List Enemy.Enemy, Random.Seed)
rollInitiative player enemies seed =
    let
        (playerInitiative, new_seed) =
            Random.step RNG.oneToTwenty seed
        new_player = Player.adjustInitiative player (playerInitiative + player.dexterity)
        (new_enemies, newest_seed) =
            List.map (\enemy -> setEnemyInitiative new_seed) enemies

        -- sortedInitiatives =
        --     List.sortBy (\(_, a) (_, b) -> compare b a) (enemyInitiatives ++ [(player, playerInitiative)])

        -- (newPlayer, newEnemies) =
        --     List.foldl
        --         (\(player, enemies) (enemy, _) ->
        --             (Player.addInitiative player enemy, enemies))
        --         (player, [])
        --         sortedInitiatives
    in
    (new_player, new_enemies, newest_seed)

setEnemyInitiative : Enemy.Enemy -> Random.Seed -> (Enemy.Enemy, Random.Seed) --TODO is this order correct?
setEnemyInitiative enemy seed =
    let
        (initiative, new_seed) =
            Random.step RNG.oneToTwenty seed
        new_inv = initiative + enemy.dexterity
    in
    ({ enemy | initiative = new_inv }, new_seed)


getAndSetInitiative : Player.Player -> List Enemy.Enemy -> Random.Seed -> (List Character, Random.Seed)
getAndSetInitiative player enemies seed =
    let
        (new_player, new_enemies, new_seed) =
            rollInitiative player enemies seed
        -- playerInitiative =
        --     Player.initiative new_player
        -- enemyInitiatives =
        --     List.map (\enemy -> (Enemy.name enemy, Enemy.initiative enemy)) new_enemies

        list = [Player new_player] ++ (List.map (\enemy -> Enemy enemy) new_enemies)
        sortedList =
            List.sortBy (\char ->
                            case char of
                                Player p ->
                                    p.turn_initiative

                                Enemy e ->
                                    e.turn_initiative
                        ) list
    in
    ([], new_seed)

getInitiative : Character -> Int
getInitiative character =
    case character of
        Player player ->
            player.turn_initiative

        Enemy enemy ->
            enemy.turn_initiative