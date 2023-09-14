module Frontend.Update exposing (update)

import Types
import Types.Player as Player
import Types.Levels as Level
import Types.Actions as Action
import Types.Player as Player
import Types.Weapons as Weapon
import Platform.Cmd as Cmd

import Logic.RandomGen as RNG
import Logic.Turn as Turn
import Random
import Task
import Time
import Types.Enemy as Enemy
import Logic.Initiative as Inv
import Types.Player as Player
import Types.Player as Player
import Backend exposing (Model)
import Svg.Styled.Attributes exposing (mode)
import Svg.Styled.Attributes exposing (letterSpacing)
import Array
import String exposing (indexes)
import Html exposing (th)
import List exposing (indexedMap)
update : Types.FrontendMsg -> Types.FrontendModel -> (Types.FrontendModel, Cmd Types.FrontendMsg)
update msg model =
    case msg of
        Types.StartGame ->
            let
                new_log = "PLAYER has started the game" :: model.event_log
            in
            ( { model | game_started = True, event_log = new_log}
            , Cmd.none
            )
        Types.ChoseRogue ->
            let
                new_player = Player.baseRogue |> Player.calculateHP
                new_log = "PLAYER has chosen to be a ROGUE" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Blade, base_weapon = Weapon.Blade, event_log = new_log }
            , Cmd.none
            )
        Types.ChoseSpy ->
            let
                new_player = Player.baseSpy |> Player.calculateHP
                new_log = "PLAYER has chosen to be a SPY" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Blade, base_weapon = Weapon.Blade, event_log = new_log }
            , Cmd.none
            )
        Types.ChoseWarrior ->
            let
                new_player = Player.baseWarrior |> Player.calculateHP
                new_log = "PLAYER has chosen to be a WARRIOR" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Spear, base_weapon = Weapon.Spear, event_log = new_log }
            , Cmd.none
            )
        Types.ChoseTank ->
            let
                new_player = Player.baseTank |> Player.calculateHP
                new_log = "PLAYER has chosen to be a TANK" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Club, base_weapon = Weapon.Club, event_log = new_log }
            , Cmd.none
            )
        Types.ReturnToClassChoice ->
            let
                new_log = List.drop 1 model.event_log
            in
            ( { model | player = Nothing, temp_player = Nothing, class_picked = False, current_weapon = Weapon.None, base_weapon = Weapon.None, event_log = new_log }
            , Cmd.none
            )
        Types.AdjustAttr attr amount->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                new_player =
                    case attr of
                        Player.Dexterity ->
                            { player | dexterity = player.dexterity + amount }
                        Player.Strength ->
                            { player | strength = player.strength + amount }
                        Player.Charisma ->
                            { player | charisma = player.charisma + amount }
                        Player.Constitution ->
                            { player | constitution = player.constitution + amount } |> Player.calculateHP
            in
            ( { model | player = Just new_player, points_to_spend = model.points_to_spend - amount }
            , Types.performMessage <| Types.CalculateHP
            )
        Types.CalculateHP ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                new_player = Player.calculateHP player
            in
            ( { model | player = Just new_player }
            , Cmd.none
            )
        Types.ConfirmPointsBuyInitial ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                new_log = "PLAYER has finalized character attributes" :: model.event_log
                (new_level, new_seed) = Level.level1 model.random_seed
            in
            ( { model | temp_player = Just player, point_buy_complete = True
                      , player_status = Player.StartedEntry, event_log = new_log, current_level = new_level, random_seed = new_seed 
                      }
            , Cmd.none
            )
        Types.ConfirmPointsBuyBetweenLevel ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                new_log = "PLAYER has finished level up improvements" :: model.event_log
            in
            ( { model | temp_player = Just player, point_buy_complete = True, event_log = new_log }
            , Cmd.none
            )
        Types.EnterBuilding ->
            let
                new_log = "PLAYER enters the building" :: model.event_log
            in
            ( { model | player_status = Player.InRoom, room_entry_type = Level.Normal, event_log = new_log }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.SearchForAnotherWayIn ->
            let
                new_log = "PLAYER attempts to find another way in" :: model.event_log
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                (random_num, new_seed) = Random.step RNG.oneToTwenty model.random_seed
                -- _ = Debug.log "Random Dex Num" random_num
                (success, attr, new_random_seed) =
                    if random_num + player.dexterity >= 18 then
                        (True, "Dex", new_seed)
                    else
                        let
                            (new_random, seed) = Random.step RNG.oneToTwenty new_seed
                            -- _ = Debug.log "Random Chr Num" new_random
                        in
                        if new_random + player.charisma >= 18 then
                            (True, "Chr", seed)
                        else
                            (False, "", seed)
                (new_player, entry_type, new_new_log) =
                    if success then
                        case model.player of
                            Just p -> 
                                if attr == "Dex" then
                                    ((p |> Player.adjustRush 2), Level.DexSneak, "PLAYER succeeded a Dexterity Save" :: new_log)
                                else if attr == "Chr" then
                                    ((p |> Player.adjustRush 2), Level.ChrSneak, "PLAYER succeeded a Charisma save" :: "PLAYER failed a Dex Save" :: new_log)
                                else 
                                    (p, Level.Normal, new_log) --SHOULD NEVER HAPPEN
                            Nothing -> (Player.defaultPlayer |> Player.calculateHP, Level.Normal, new_log) --SHOULD NEVER HAPPEN
                    else
                        case model.player of
                            Just p -> ((p |> Player.adjustHealth -5), Level.FailedSneak, ("PLAYER loses 5 health") :: ("PLAYER failed both the Dexterity and Charisma saves" :: new_log))
                            Nothing -> (Player.defaultPlayer |> Player.calculateHP, Level.Normal, new_log) --SHOULD NEVER HAPPEN
            in
            ( { model | player = Just new_player, player_status = Player.InRoom, room_entry_type = entry_type, random_seed = new_random_seed, event_log = new_new_log }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.EnterRoom ->
            let
                new_log = "********PLAYER enters the room********" :: model.event_log
                new_room_num = model.current_room + 1
                new_room = List.filter (\r -> r.num == new_room_num) model.current_level.rooms |> List.head |> Maybe.withDefault Level.defaultRoom
                enemy_num = new_room.enemies |> List.length
            in
            ( { model | player_status = Player.InRoom, current_room = new_room_num, event_log = new_log
                      , distance_from_enemy = Action.Range, current_room_enemy_num = enemy_num, selected_enemy_id = 1 }
            , Types.performMessage <| Types.StartRound
            )
        Types.StartRound ->
            let
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer --SHOULD NEVER HAPPEN
                player =
                    if new_player.sanity < 10 then
                        new_player |> Player.adjustHealth (new_player.sanity - 10)
                    else
                        new_player
                enemies = 
                    case model.current_level.rooms |> List.filter (\r -> r.num == model.current_room) |> List.head of
                        Just r -> r.enemies |> List.filter (\e -> e.hp > 0)
                        Nothing -> []
                new_log = "Round Start-------------------------" :: model.event_log
                new_enemy_taunted =
                    if Tuple.second model.enemy_taunted > 0 then
                       (Tuple.first model.enemy_taunted, Tuple.second model.enemy_taunted - 1)
                    else
                        (False, 0)
                new_player_taunt_cooldown =
                    if model.player_taunt_cooldown > 0 then
                        model.player_taunt_cooldown - 1
                    else
                        0
                new_player_stealthed =
                    if Tuple.second model.player_stealthed > 0 then
                        (Tuple.first model.player_stealthed, Tuple.second model.player_stealthed - 1)
                    else
                        (False, 0)
                new_player_stealth_cooldown =
                    if model.player_stealth_cooldown > 0 then
                        model.player_stealth_cooldown - 1
                    else
                        0
                
                new_furious_attack_cooldown =
                    if model.furious_attack_cooldown > 0 then
                        model.furious_attack_cooldown - 1
                    else
                        0
                
                new_self_heal_cooldown =
                    if model.self_heal_cooldown > 0 then
                        model.self_heal_cooldown - 1
                    else
                        0
            in
            if player.hp == 0 then
                ( model
                , Types.performMessage <| Types.GameOver
                )
            else if List.length enemies < 1 then
                ( model
                , Types.performMessage <| Types.FinishRoom
                )
            else if player.rush > 0 then --Player free action with rush
                let
                    new_new_log = "PLAYER has a free action" :: new_log
                    (turn_order, new_seed) = Inv.rollInitiative player [] model.random_seed
                    player_adjust_rush = player |> Player.adjustRush -1
                in
                ( { model | round_turn_list = turn_order, random_seed = new_seed, event_log = new_new_log, player = Just player_adjust_rush
                          , enemy_taunted = new_enemy_taunted
                          , player_taunt_cooldown = new_player_taunt_cooldown
                          , player_stealth_cooldown = new_player_stealth_cooldown
                          , player_stealthed = new_player_stealthed
                          , furious_attack_cooldown = new_furious_attack_cooldown
                          , self_heal_cooldown = new_self_heal_cooldown 
                  }
                , Types.performMessage <| Types.NextTurn
                )
            else if (List.any (\e -> e.rush > 0) enemies) then --Enemy free action with rush
                let
                    enemies_with_rush = List.filter (\e -> e.rush > 0) enemies
                    new_new_log = 
                        List.map (\e -> "ENEMY " ++ (String.fromInt e.id) ++ " has a free action") enemies_with_rush |> List.append new_log
                    (turn_order, new_seed) = Inv.rollInitiative player enemies_with_rush model.random_seed
                    adjusted_turn_order = 
                        List.filter (\(t, _) -> 
                                        case t of 
                                            Inv.Player _ -> False
                                            Inv.Enemy e -> e.rush > 0
                                    ) turn_order
                    new_enemies = List.map (\e -> e |> Enemy.adjustRush -1) enemies
                    new_room = 
                        case model.current_level.rooms |> List.filter (\r -> r.num == model.current_room) |> List.head of
                            Just r -> { r | enemies = new_enemies }
                            Nothing -> Level.defaultRoom
                    all_rooms = 
                        List.map (\r -> if r.num == model.current_room then new_room else r) model.current_level.rooms
                    the_current_level = model.current_level
                    new_current_level = 
                        { the_current_level | rooms = all_rooms }
                in
                ( { model | round_turn_list = adjusted_turn_order, random_seed = new_seed, event_log = new_new_log, current_level = new_current_level
                          , enemy_taunted = new_enemy_taunted
                          , player_taunt_cooldown = new_player_taunt_cooldown
                          , player_stealth_cooldown = new_player_stealth_cooldown
                          , player_stealthed = new_player_stealthed
                          , furious_attack_cooldown = new_furious_attack_cooldown
                          , self_heal_cooldown = new_self_heal_cooldown 
                  }
                , Types.performMessage <| Types.NextTurn
                )
            else --Normal round
                let
                    (turn_order, new_seed) = Inv.rollInitiative player enemies model.random_seed
                    new_turn_order = List.reverse turn_order
                in
                ( { model | round_turn_list = new_turn_order, random_seed = new_seed, event_log = new_log
                          , enemy_taunted = new_enemy_taunted
                          , player_stealthed = new_player_stealthed
                          , furious_attack_cooldown = new_furious_attack_cooldown
                          , self_heal_cooldown = new_self_heal_cooldown 
                  }
                , Types.performMessage <| Types.NextTurn
                )
        Types.NextTurn ->
            let
                next_turn = List.head model.round_turn_list
                new_list = List.drop 1 model.round_turn_list
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer --SHOULD NEVER HAPPEN

                enemies = 
                    case model.current_level.rooms |> List.filter (\r -> r.num == model.current_room) |> List.head of
                        Just r -> r.enemies |> List.filter (\e -> e.hp > 0)
                        Nothing -> []
            in
            if player.hp == 0 then
                ( model
                , Types.performMessage <| Types.GameOver
                )
            else if List.length enemies < 1 then
                ( model
                , Types.performMessage <| Types.FinishRoom
                )
            else
                case next_turn of
                    Just (Inv.Player _, inv) ->
                        let
                            new_log = ("PLAYER turn with initiative of " ++ String.fromInt inv) :: model.event_log
                        in
                        -- Wait for player action
                        ( { model | round_turn_list = new_list, show_player_action_options = True, event_log = new_log }
                        , Cmd.none
                        )
                    Just (Inv.Enemy e, inv) ->
                        let
                            new_log = ("ENEMY " ++ (String.fromInt e.id) ++ " turn with initiative of " ++ String.fromInt inv) :: model.event_log
                        in
                        -- Do Enemy Action
                        ( { model | round_turn_list = new_list, event_log = new_log}
                        , Types.performMessage <| Types.EnemyAction e
                        )
                    Nothing ->
                        -- Round is over
                        ( model
                        , Types.performMessage <| Types.StartRound
                        )
        Types.FinishRoom ->
            let
                (new_status, command) =
                    if (List.length model.current_level.rooms) == model.current_room then
                        (Player.BetweenLevels, Types.performMessage <| Types.FinishLevel)
                    else
                        (Player.BetweenRooms, Cmd.none)
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer --SHOULD NEVER HAPPEN
                new_player = Player.adjustRush -100 player

                new_log = "********PLAYER has finished the room********" :: model.event_log
                (new_new_log, new_weapon) =
                    if model.current_weapon == model.base_weapon then
                        (new_log, model.current_weapon)
                    else
                        (("PLAYER has regained " ++ (Weapon.weaponToString model.base_weapon)) :: new_log
                        , model.base_weapon
                        )
            in
            ( { model | player_status = new_status, event_log = new_new_log, current_weapon = new_weapon, player = Just new_player
                      , enemy_taunted = (False, 0)
                      , player_stealthed = (False, 0) 
                      }
            , command
            )
        Types.FinishLevel ->
            let
                new_log = "PLAYER has finished the level" :: model.event_log
                ((new_level, new_seed), status) =
                    case model.current_level.level of
                        1 -> (Level.level2 model.random_seed, Player.BetweenLevels)
                        2 -> (Level.level3 model.random_seed, Player.BetweenLevels)
                        3 -> (Level.level4 model.random_seed, Player.BetweenLevels)
                        4 -> ((model.current_level, model.random_seed), Player.Finished)
                        _ -> ((model.current_level, model.random_seed), Player.Finished)
            in
            ( { model | player_status = status, current_level = new_level, current_room = 0
                      , event_log = new_log, points_to_spend = 3, point_buy_complete = False
                      , random_seed = new_seed
                      }
            , Cmd.none
            )
        Types.BetweenRoomRest room->
            let
                new_log = "PLAYER has chosen to rest" :: model.event_log
                room_enemies = model.current_room_enemy_num
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                (updated_player, new_seed) = Action.afterRoomEffect Action.Rest room_enemies model.random_seed new_player
                health_regained = updated_player.hp - new_player.hp
                sanity_regained = updated_player.sanity - new_player.sanity
                new_new_log = ("PLAYER has regained " ++ (String.fromInt health_regained) ++ " health and " ++ (String.fromInt sanity_regained) ++ " sanity") :: new_log
                new_enemy_taunted =
                    if Tuple.second model.enemy_taunted > 0 then
                       (Tuple.first model.enemy_taunted, Tuple.second model.enemy_taunted - 1)
                    else
                        (False, 0)

                new_player_stealthed =
                    if Tuple.second model.player_stealthed > 0 then
                        (Tuple.first model.player_stealthed, Tuple.second model.player_stealthed - 1)
                    else
                        (False, 0)
                
                new_furious_attack_cooldown =
                    if model.furious_attack_cooldown > 0 then
                        model.furious_attack_cooldown - 1
                    else
                        0
                
                new_self_heal_cooldown =
                    if model.self_heal_cooldown > 0 then
                        model.self_heal_cooldown - 1
                    else
                        0
            in
            ( { model | player = Just updated_player, random_seed = new_seed, event_log = new_new_log
                      , enemy_taunted = new_enemy_taunted
                      , player_stealthed = new_player_stealthed
                      , furious_attack_cooldown = new_furious_attack_cooldown
                      , self_heal_cooldown = new_self_heal_cooldown
              }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.BetweenRoomLoot room ->
            let
                new_log = "PLAYER has chosen to loot" :: model.event_log
                room_enemies = model.current_room_enemy_num
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                
                (updated_player, new_seed) = Action.afterRoomEffect Action.Loot room_enemies model.random_seed new_player
                gained_coins = updated_player.coins - new_player.coins
                new_new_log = ("PLAYER has gained " ++ (String.fromInt gained_coins) ++ " coins") :: new_log
            in
            ( { model | player = Just updated_player, random_seed = new_seed, event_log = new_new_log }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.BetweenRoomRush room ->
            let
                new_log = "PLAYER has chosen to rush into the next room" :: model.event_log
                new_new_log = "Player gains one free action in the next room" :: new_log
                room_enemies = model.current_room_enemy_num
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN

                (updated_player, new_seed) = Action.afterRoomEffect Action.Rush room_enemies model.random_seed new_player
            in
            ( { model | player = Just updated_player, random_seed = new_seed, event_log = new_new_log }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.PurchaseWater ->
            let
                new_log = "PLAYER has purchased water (recover 5 hp)" :: model.event_log
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                updated_player = new_player |> Player.adjustHealth 5 |> Player.adjustCoins -2
            in
            ( { model | player = Just updated_player, event_log = new_log }
            , Cmd.none
            )
        Types.PurchaseJuice ->
            let
                new_log = "PLAYER has purchased juice (recover 10 sanity)" :: model.event_log
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                updated_player = new_player |> Player.adjustSanity 10 |> Player.adjustCoins -5
            in
            ( { model | player = Just updated_player, event_log = new_log }
            , Cmd.none
            )
        Types.PurchaseHotChocolate ->
            let
                new_log = "PLAYER has purchased hot chocolate (fully recover hp and sanity)" :: model.event_log
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                updated_player = new_player |> Player.adjustHealth 1000 |> Player.adjustSanity 1000 |> Player.adjustCoins -10
            in
            ( { model | player = Just updated_player, event_log = new_log }
            , Cmd.none
            )
        Types.PurchaseProteinShake ->
            let
                new_log = "PLAYER has purchased Protein Shake (Gain 10 max hp)" :: model.event_log
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                updated_player = new_player |> Player.adjustMaxHealth 10 |> Player.adjustHealth 10 |> Player.adjustCoins -20
            in
            ( { model | player = Just updated_player, event_log = new_log }
            , Cmd.none
            )
        Types.BetweenLevelPurchaseFinished ->
            let
                new_log = "PLAYER has finished purchasing from the vending machine" :: model.event_log
                new_new_log = "------->PLAYER goes up to the next level------->" :: new_log
            in
            ( { model | event_log = new_new_log }
            , Types.performMessage <| Types.EnterRoom
            )

        -- PLAYER ACTIONS
        Types.SelectEnemy id ->
            ( { model | selected_enemy_id = id }
            , Cmd.none
            )
        Types.PlayerAttack distance mod->
            -- TODO lose weapon on ranged attack?
            -- TODO how to chose which enemy to attack?
            let
                the_current_level = model.current_level
                (attack_type, new_weapon) = 
                    if distance == Action.Melee then
                        (Weapon.Melee, model.current_weapon)
                    else
                        (Weapon.Ranged, Weapon.None)
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                (dmg, new_seed) = (Weapon.getWeaponDamage (Weapon.Player player) model.current_weapon attack_type) model.random_seed
                damage_dealt = dmg * mod
                new_log = ("PLAYER attacks with " ++ (Weapon.weaponToString model.current_weapon) ++ " for " ++ (String.fromInt damage_dealt) ++ " damage") :: model.event_log
                room = 
                    List.filter (\r -> r.num == model.current_room) model.current_level.rooms |> List.head |> Maybe.withDefault Level.defaultRoom
                -- new_enemies = List.head room.enemies |> Maybe.withDefault Enemy.defaultEnemy |> Enemy.adjustHealth damage_dealt--TODO FIGURE OUT ENEMY STUFF HERE!!!
                new_enemies_list = --TODO select enemy to attack
                    List.map (\e -> 
                                if e.id == model.selected_enemy_id then 
                                    (Enemy.adjustHealth (damage_dealt * -1) e)
                                else 
                                    e
                            ) room.enemies
                    -- case room.enemies of
                    --     [] -> []
                    --     (h::t) -> 
                    --         let
                    --             new_enemy = 
                    --                 case ( h) of
                    --                     Nothing -> []
                    --                     Just e -> [e]
                    --         in
                    --         new_enemy ++ t
                new_enemies =
                    List.filter (\e -> e.hp > 0) new_enemies_list
                (new_player, new_new_log) =
                    if (List.length new_enemies) < (List.length room.enemies) then
                        (Player.adjustSanity -3 player, "PLAYER has killed an enemy and lost 3 sanity" :: new_log)
                    else
                        (player, new_log)
                new_room = 
                    { room | enemies = new_enemies }
                all_rooms = 
                    List.map (\r -> if r.num == model.current_room then new_room else r) model.current_level.rooms
                new_current_level = 
                    { the_current_level | rooms = all_rooms }
            in 
            ( { model | current_level = new_current_level, show_player_action_options = False
                      , event_log = new_new_log, current_weapon = new_weapon, player = Just new_player
                      , random_seed = new_seed
                     }
            , Types.performMessage <| Types.NextTurn
            )

        Types.PlayerMove direction ->
            let
                new_log = 
                    case direction of
                        Action.Toward ->
                            "PLAYER moves TOWARD the enemy" :: model.event_log
                        Action.Away ->
                            "Player moves AWAY from the enemy" :: model.event_log
            in
            if direction == Action.Toward then
                ( { model | distance_from_enemy = Action.Melee, show_player_action_options = False, event_log = new_log }
                , Types.performMessage <| Types.NextTurn
                )
            else
                ( { model | distance_from_enemy = Action.Range, show_player_action_options = False, event_log = new_log }
                , Types.performMessage <| Types.NextTurn
                )
        Types.PlayerTaunt ->
            let
                new_log = "PLAYER taunts the enemy" :: model.event_log
                player =
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                dur = 
                    if player.class == Player.Spy then
                        4
                    else
                        3
            in
            ( { model | show_player_action_options = False, event_log = new_log, enemy_taunted = (True, dur), player_taunt_cooldown = 5 }
            , Types.performMessage <| Types.NextTurn
            )
        Types.PlayerFuriousAttack ->
            let
                new_log = "PLAYER performs a furious attack" :: model.event_log
                player =
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                mod = 
                    if player.class == Player.Warrior then
                        2
                    else
                        1.5
            in
            -- TODO implement cooldown and negative effect
            ( { model | show_player_action_options = False, event_log = new_log, furious_attack_cooldown = 5 }
            , Types.performMessage <| Types.PlayerAttack model.distance_from_enemy 2
            )
        Types.PlayerStealth ->
            let
                new_log = "PLAYER enters STEALTH" :: model.event_log
                player =
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                dur = 
                    if player.class == Player.Rogue then
                        4
                    else
                        3
            in
            ( { model | show_player_action_options = False, event_log = new_log, player_stealthed = (True, dur), player_stealth_cooldown = 5 }
            , Types.performMessage <| Types.NextTurn
            )
        Types.PlayerHeal ->
            let
                new_log = "PLAYER performs SELF HEAL" :: model.event_log
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
                heal_amount = round (toFloat player.sanity / 10)
                new_player =
                    if player.class == Player.Tank then
                        player |> Player.adjustHealth (heal_amount + 5)
                    else
                        player |> Player.adjustHealth heal_amount
                heal_report = new_player.hp - player.hp
                new_new_log = ("PLAYER heals for " ++ (String.fromInt heal_report) ++ " health") :: new_log
            in
            -- TODO cooldowns
            ( { model | show_player_action_options = False, event_log = new_new_log, self_heal_cooldown = 5, player = Just new_player }
            , Types.performMessage <| Types.NextTurn
            )
        
        Types.EnemyAction enemy ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer |> Player.calculateHP --SHOULD NEVER HAPPEN
            in
            if player.hp == 0 then
                ( model
                , Types.performMessage <| Types.GameOver
                )
            else if enemy.hp == 0 then --SHOULD NEVER HAPPEN
                ( model
                , Types.performMessage <| Types.NextTurn
                )
            else
                let
                    (possible_actions, weapon) = Action.getEnemyActions enemy model.distance_from_enemy
                    (rand_num, new_seed) = Random.step RNG.zeroToNine model.random_seed
                    (sanity1_roll, sanity2_roll, new_seed1) = 
                        let
                            (roll1, seed1) = Random.step RNG.oneToHundred new_seed
                            (roll2, seed2) = Random.step RNG.oneToHundred seed1
                        in
                        case (roll1 < (player.sanity + player.charisma), roll2 < player.sanity) of
                            (True, True) -> (True, True, seed2)
                            (True, False) -> (True, False, seed2)
                            (False, True) -> (False, True, seed2)
                            (False, False) -> (False, False, seed2)
                    action = Array.fromList possible_actions |> Array.get rand_num |> Maybe.withDefault Action.EnemyTaunt
                    (new_player, new_distance, (new_log, newest_seed)) =
                        case action of
                            Action.EnemyRangedAttack ->
                                let
                                    (d, returned_seed) = (Weapon.getWeaponDamage (Weapon.Enemy enemy) weapon Weapon.Ranged new_seed1)
                                    dmg = toFloat d
                                    new_dmg =
                                        if Tuple.first model.enemy_taunted && sanity1_roll then
                                            dmg / 2
                                        else
                                            dmg
                                    new_new_dmg =
                                        if Tuple.first model.player_stealthed && sanity2_roll then
                                            new_dmg / 2
                                        else
                                            new_dmg
                                    rounded_dmg = round new_new_dmg
                                    p = Player.adjustHealth (rounded_dmg * -1) player
                                in
                                    (p, model.distance_from_enemy, (("ENEMY " ++ (String.fromInt enemy.id) ++ " attacks with " ++ (Weapon.weaponToString weapon) ++ " for " ++ (String.fromInt rounded_dmg) ++ " damage") :: model.event_log, returned_seed))

                            Action.EnemyMeleeAttack ->
                                let
                                    (d, returned_seed) = (Weapon.getWeaponDamage (Weapon.Enemy enemy) weapon Weapon.Melee new_seed1)
                                    dmg = toFloat d
                                    new_dmg =
                                        if Tuple.first model.enemy_taunted && sanity1_roll then
                                            dmg / 2
                                        else
                                            dmg
                                    new_new_dmg =
                                        if Tuple.first model.player_stealthed && sanity2_roll then
                                            new_dmg / 2
                                        else
                                            new_dmg
                                    rounded_dmg = round new_new_dmg
                                    p = Player.adjustHealth (rounded_dmg * -1) player
                                in
                                    (p, model.distance_from_enemy, (("ENEMY " ++ (String.fromInt enemy.id) ++ " attacks with " ++ (Weapon.weaponToString weapon) ++ " for " ++ (String.fromInt rounded_dmg) ++ " damage") :: model.event_log, returned_seed))

                            Action.EnemyMove direction->
                                if direction == Action.Toward then
                                    ( player, Action.Melee, (("ENEMY " ++ (String.fromInt enemy.id) ++ " moves TOWARD the player") :: model.event_log, new_seed1))
                                else
                                    ( player, Action.Range, (("ENEMY " ++ (String.fromInt enemy.id) ++ " moves AWAY from the player") :: model.event_log, new_seed1))

                            Action.EnemyTaunt ->
                                let
                                    sanity_dmg = (Weapon.getTauntDamage enemy) + 2
                                    p = Player.adjustSanity (sanity_dmg * -1) player
                                in
                                    (p, model.distance_from_enemy, (("ENEMY " ++ (String.fromInt enemy.id) ++ " taunts the player for " ++ (String.fromInt sanity_dmg) ++ " sanity damage") :: model.event_log, new_seed1))

                in
                ( { model | random_seed = newest_seed, player = Just new_player, distance_from_enemy = new_distance, event_log = new_log }
                , Types.performMessage <| Types.NextTurn
                )
        Types.ShowHelp ->
            ( { model | show_help_menu = not model.show_help_menu }
            , Cmd.none
            )

        Types.GameOver ->
            let
                new_log = "PLAYER has died" :: model.event_log
            in
            ( { model | player_status = Player.Lost, event_log = new_log }
            , Cmd.none
            )

        Types.ResetGame -> --TODO check if these are all the needed fields
            ( { model | game_started = False
                      , player = Nothing
                      , temp_player = Nothing
                      , class_picked = False
                      , points_to_spend = 10
                      , point_buy_complete = False
                      , player_status = Player.NotStarted
                      , current_level = Level.level0
                      , current_room = 0
                      , player_stealthed = (False, 0)
                      , player_stealth_cooldown = 0
                      , enemy_taunted = (False, 0)
                      , player_taunt_cooldown = 0
                      , furious_attack_cooldown = 0
                      , self_heal_cooldown = 0
                      , event_log = ["RESET GAME"]
             }
            , Cmd.none
            )
        Types.JumpToFinish ->
            ( { model | player_status = Player.Finished }
            , Cmd.none
            )
        Types.GetCleanTime time ->
            let
                millis = Time.posixToMillis time
            in
            ( { model | random_seed = Random.initialSeed millis }
            , Cmd.none
            )
        Types.TestChangeDistance ->
            let
                new_distance = 
                    if model.distance_from_enemy == Action.Melee then
                        Action.Range
                    else
                        Action.Melee
            in
            ( { model | distance_from_enemy = new_distance }
            , Cmd.none
            )
        _ ->
            ( model
            , Cmd.none
            )