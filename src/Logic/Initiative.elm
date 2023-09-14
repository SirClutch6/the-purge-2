module Logic.Initiative exposing (..)

import Types.Player as Player
import Types.Enemy as Enemy
import Random
import Logic.RandomGen as RNG
import List
import Dict

type Character
    = Player Player.Player
    | Enemy Enemy.Enemy

type alias CharacterWithInitiative =
    (Character, Int)

rollInitiative : Player.Player -> List Enemy.Enemy -> Random.Seed -> (List CharacterWithInitiative, Random.Seed)
rollInitiative player enemies seed =
    let
        (playerInitiative, new_seed) =
            Random.step RNG.oneToTwenty seed
        player_inv = playerInitiative + player.dexterity
        new_player = (Player (Player.adjustInitiative player player_inv), player_inv)

        (enemy_inv_list, new_new_seed) = Random.step (RNG.randomListGen (List.length enemies) RNG.oneToTwenty) new_seed
        combined_enemy_list = List.map2 Tuple.pair enemies enemy_inv_list
        new_enemies = List.foldl setEnemyInitiative [] combined_enemy_list
        final_list = sortInvValues <| new_player :: new_enemies
    in
    (final_list, new_new_seed)

setEnemyInitiative : (Enemy.Enemy, Int) -> List CharacterWithInitiative -> List CharacterWithInitiative
setEnemyInitiative (enemy, inv) list =
    let
        new_inv = inv + enemy.dexterity
    in
    (Enemy (Enemy.adjustInitiative enemy new_inv), new_inv) :: list

sortInvValues : List CharacterWithInitiative -> List CharacterWithInitiative
sortInvValues list =
    List.sortBy Tuple.second list


-- getAndSetInitiative : Player.Player -> List Enemy.Enemy -> Random.Seed -> (List Character, Random.Seed)
-- getAndSetInitiative player enemies seed =
--     let
--         (inv_list, new_seed) =
--             rollInitiative player enemies seed
--         -- playerInitiative =
--         --     Player.initiative new_player
--         -- enemyInitiatives =
--         --     List.map (\enemy -> (Enemy.name enemy, Enemy.initiative enemy)) new_enemies

--         -- list = [Player new_player] ++ (List.map (\enemy -> Enemy enemy) new_enemies)
--         -- sortedList =
--         --     List.sortBy (\char ->
--         --                     case char of
--         --                         Player p ->
--         --                             p.turn_initiative

--         --                         Enemy e ->
--         --                             e.turn_initiative
--         --                 ) list
--         inv_dict = Dict.empty |> addToDict (Player new_player)

--     in
--     ([], new_seed)

-- addToDict : Character -> Dict.Dict Int Int -> Dict.Dict Int Int
-- addToDict char dict =
--     case char of
--         Player p ->
--             Dict.insert -1 p.turn_initiative dict
--         Enemy e ->
--             Dict.insert e.id e.turn_initiative dict
-- getInitiative : Character -> Int
-- getInitiative character =
--     case character of
--         Player player ->
--             player.turn_initiative

--         Enemy enemy ->
--             enemy.turn_initiative