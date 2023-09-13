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
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Blade, event_log = new_log }
            , Cmd.none
            )
        Types.ChoseSpy ->
            let
                new_player = Player.baseSpy |> Player.calculateHP
                new_log = "PLAYER has chosen to be a SPY" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Blade, event_log = new_log }
            , Cmd.none
            )
        Types.ChoseWarrior ->
            let
                new_player = Player.baseWarrior |> Player.calculateHP
                new_log = "PLAYER has chosen to be a WARRIOR" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Spear, event_log = new_log }
            , Cmd.none
            )
        Types.ChoseTank ->
            let
                new_player = Player.baseTank |> Player.calculateHP
                new_log = "PLAYER has chosen to be a TANK" :: model.event_log
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True, current_weapon = Weapon.Club, event_log = new_log }
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
        Types.ConfirmPointsBuy ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                new_log = "PLAYER has finalized character attributes" :: model.event_log
            in
            ( { model | temp_player = Just player, point_buy_complete = True, player_status = Player.StartedEntry, event_log = new_log }
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
                                    ((p |> Player.adjustRush 2), Level.ChrSneak, "Player has succeeded a Charisma save" :: "PLAYER failed a Dex Save" :: new_log)
                                else 
                                    (p, Level.Normal, new_log) --SHOULD NEVER HAPPEN
                            Nothing -> (Player.defaultPlayer |> Player.calculateHP, Level.Normal, new_log) --SHOULD NEVER HAPPEN
                    else
                        case model.player of
                            Just p -> ((p |> Player.adjustHealth -5), Level.FailedSneak, "PLAYER failed both the Dexterity and Charisma saves" :: new_log)
                            Nothing -> (Player.defaultPlayer |> Player.calculateHP, Level.Normal, new_log) --SHOULD NEVER HAPPEN
            in
            ( { model | player = Just new_player, player_status = Player.InRoom, room_entry_type = entry_type, random_seed = new_random_seed, event_log = new_new_log }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.EnterRoom ->
            let
                new_log = "PLAYER enters the room" :: model.event_log
            in
            ( { model | player_status = Player.InRoom, current_room = model.current_room + 1, event_log = new_log }
            , Types.performMessage <| Types.StartRound
            )
        Types.StartRound ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.defaultPlayer --SHOULD NEVER HAPPEN
                enemies = 
                    case model.current_level.rooms |> List.filter (\r -> r.num == model.current_room) |> List.head of
                        Just r -> r.enemies |> List.filter (\e -> e.hp > 0)
                        Nothing -> []
                new_log = "Round Start" :: model.event_log
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
                let
                    (turn_order, new_seed) = Inv.rollInitiative player enemies model.random_seed
                    new_turn_order = List.reverse turn_order
                in
                ( { model | round_turn_list = new_turn_order, random_seed = new_seed, event_log = new_log }
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
                new_status =
                    if (List.length model.current_level.rooms) == model.current_room then
                        Player.BetweenLevels
                    else
                        Player.BetweenRooms

                new_log = "PLAYER has finished the room" :: model.event_log
            in
            ( { model | player_status = new_status, event_log = new_log }
            , Cmd.none
            )
        Types.FinishLevel ->
            let
                new_log = "PLAYER has finished the level" :: model.event_log
                (new_level, status) =
                    case model.current_level.level of
                        1 -> (Level.level2, Player.BetweenLevels)
                        2 -> (Level.level3, Player.BetweenLevels)
                        3 -> (Level.level4, Player.BetweenLevels)
                        4 -> (model.current_level, Player.Finished)
                        _ -> (model.current_level, Player.Finished)
            in
            ( { model | player_status = status, current_level = new_level, current_room = 0, event_log = new_log, points_to_spend = 3, point_buy_complete = False }
            , Cmd.none
            )
        Types.BetweenRoomRest room->
            let
                new_log = "PLAYER has chosen to rest" :: model.event_log
                room_enemies = room.enemies
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                (updated_player, new_seed) = Action.afterRoomEffect Action.Rest room_enemies model.random_seed new_player
            in
            ( { model | player = Just updated_player, random_seed = new_seed, event_log = new_log }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.BetweenRoomLoot room ->
            let
                new_log = "PLAYER has chosen to loot" :: model.event_log
                room_enemies = room.enemies
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
                new_new_log = "Player gains two free actions in the next room" :: new_log
                room_enemies = room.enemies
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN

                (updated_player, new_seed) = Action.afterRoomEffect Action.Rush room_enemies model.random_seed new_player
            in
            ( { model | player = Just updated_player, random_seed = new_seed, event_log = new_new_log }
            , Types.performMessage <| Types.EnterRoom
            )

        -- PLAYER ACTIONS
        Types.PlayerAttack distance mod->
            -- TODO lose weapon on ranged attack?
            -- TODO how to chose which enemy to attack?
            let
                the_current_level = model.current_level
                attack_type = 
                    if distance == Action.Melee then
                        Weapon.Melee
                    else
                        Weapon.Ranged
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                damage_dealt = (Weapon.getWeaponDamage (Weapon.Player player) model.current_weapon attack_type) * mod
                new_log = ("PLAYER attacks with " ++ (Weapon.weaponToString model.current_weapon) ++ " for " ++ (String.fromInt damage_dealt) ++ " damage") :: model.event_log
                room = 
                    List.filter (\r -> r.num == model.current_room) model.current_level.rooms |> List.head |> Maybe.withDefault Level.defaultRoom
                -- new_enemies = List.head room.enemies |> Maybe.withDefault Enemy.defaultEnemy |> Enemy.adjustHealth damage_dealt--TODO FIGURE OUT ENEMY STUFF HERE!!!
                new_enemies = --TODO select enemy to attack
                    case room.enemies of
                        [] -> []
                        (h::t) -> 
                            let
                                new_enemy = 
                                    case (Enemy.adjustHealth (damage_dealt * -1) h) of
                                        Nothing -> []
                                        Just e -> [e]
                            in
                            new_enemy ++ t
                new_room = 
                    { room | enemies = new_enemies }
                all_rooms = 
                    List.map (\r -> if r.num == model.current_room then new_room else r) model.current_level.rooms
                new_current_level = 
                    { the_current_level | rooms = all_rooms }
            in 
            ( { model | current_level = new_current_level, show_player_action_options = False, event_log = new_log }
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
            in
            -- TODO Taunt here
            ( { model | show_player_action_options = False, event_log = new_log }
            , Types.performMessage <| Types.NextTurn
            )
        Types.PlayerFuriousAttack ->
            let
                new_log = "PLAYER performs a furious attack" :: model.event_log
            in
            -- TODO implement cooldown and negative effect
            ( { model | show_player_action_options = False, event_log = new_log }
            , Types.performMessage <| Types.PlayerAttack model.distance_from_enemy 1
            )
        Types.PlayerStealth ->
            let
                new_log = "PLAYER enters STEALTH" :: model.event_log
            in
            -- TODO Stealth here
            ( { model | show_player_action_options = False, event_log = new_log }
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
            in
            -- TODO cooldowns
            ( { model | show_player_action_options = False, event_log = new_log }
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
                    action = Array.fromList possible_actions |> Array.get rand_num |> Maybe.withDefault Action.EnemyTaunt
                    (new_player, new_distance, new_log) =
                        case action of
                            Action.EnemyRangedAttack ->
                                let
                                    dmg = Weapon.getWeaponDamage (Weapon.Enemy enemy) weapon Weapon.Ranged
                                    p = Player.adjustHealth (dmg * -1) player
                                in
                                    (p, model.distance_from_enemy, ("ENEMY " ++ (String.fromInt enemy.id) ++ " attacks with " ++ (Weapon.weaponToString weapon) ++ " for " ++ (String.fromInt dmg) ++ " damage") :: model.event_log)

                            Action.EnemyMeleeAttack ->
                                let
                                    dmg = Weapon.getWeaponDamage (Weapon.Enemy enemy) weapon Weapon.Melee
                                    p = Player.adjustHealth (dmg * -1) player
                                in
                                    (p, model.distance_from_enemy, ("ENEMY " ++ (String.fromInt enemy.id) ++ " attacks with " ++ (Weapon.weaponToString weapon) ++ " for " ++ (String.fromInt dmg) ++ " damage") :: model.event_log)

                            Action.EnemyMove direction->
                                if direction == Action.Toward then
                                    ( player, Action.Melee, ("ENEMY " ++ (String.fromInt enemy.id) ++ " moves TOWARD the player") :: model.event_log)
                                else
                                    ( player, Action.Range, ("ENEMY " ++ (String.fromInt enemy.id) ++ " moves AWAY from the player") :: model.event_log)

                            Action.EnemyTaunt ->
                                let
                                    sanity_dmg = Weapon.getTauntDamage enemy
                                    p = Player.adjustSanity (sanity_dmg * -1) player
                                in
                                    (p, model.distance_from_enemy, ("ENEMY " ++ (String.fromInt enemy.id) ++ " taunts the player for " ++ (String.fromInt sanity_dmg) ++ " sanity damage") :: model.event_log)

                in
                ( { model | random_seed = new_seed, player = Just new_player, distance_from_enemy = new_distance, event_log = new_log }
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
                      , current_level = Level.level1
                      , current_room = 0
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
        _ ->
            ( model
            , Cmd.none
            )