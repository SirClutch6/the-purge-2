module Frontend.Update exposing (update)

import Types
import Types.Player as Player
import Types.Levels as Level
import Types.Actions as Action
import Types.Player as Player
import Types.Player as Player
import Types.Player as Player
import Platform.Cmd as Cmd

import Types.RandomGen as RNG
import Random
import Task
import Time

update : Types.FrontendMsg -> Types.FrontendModel -> (Types.FrontendModel, Cmd Types.FrontendMsg)
update msg model =
    case msg of
        Types.StartGame ->
            ( { model | game_started = True}
            , Cmd.none
            )
        Types.ChoseRogue ->
            let
                new_player = Player.baseRogue |> Player.calculateHP
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True }
            , Cmd.none
            )
        Types.ChoseSpy ->
            let
                new_player = Player.baseSpy |> Player.calculateHP
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True }
            , Cmd.none
            )
        Types.ChoseWarrior ->
            let
                new_player = Player.baseWarrior |> Player.calculateHP
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True }
            , Cmd.none
            )
        Types.ChoseTank ->
            let
                new_player = Player.baseTank |> Player.calculateHP
            in
            ( { model | player = Just new_player, temp_player = Just new_player, class_picked = True }
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
            in
            ( { model | temp_player = Just player, point_buy_complete = True, player_status = Player.StartedEntry }
            , Cmd.none
            )
        Types.EnterBuilding ->
            ( { model | player_status = Player.InRoom, room_entry_type = Level.Normal }
            , Cmd.none
            )
        Types.SearchForAnotherWayIn ->
            let
                player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                (random_num, new_seed) = Random.step RNG.oneToTwenty model.random_seed
                (success, attr, new_random_seed) =
                    if random_num + player.dexterity >= 15 then
                        (True, "Dex", new_seed)
                    else
                        let
                            (new_random, seed) = Random.step RNG.oneToTwenty new_seed
                        in
                        if new_random + player.charisma >= 15 then
                            (True, "Chr", seed)
                        else
                            (False, "", seed)
                (new_player, entry_type) =
                    if success then
                        case model.player of
                            Just p -> 
                                if attr == "Dex" then
                                    ((p |> Player.adjustRush 2), Level.DexSneak)
                                else if attr == "Chr" then
                                    ((p |> Player.adjustRush 2), Level.ChrSneak)
                                else 
                                    (p, Level.Normal) --SHOULD NEVER HAPPEN
                            Nothing -> (Player.defaultPlayer |> Player.calculateHP, Level.Normal) --SHOULD NEVER HAPPEN
                    else
                        case model.player of
                            Just p -> ((p |> Player.adjustHealth -5), Level.FailedSneak)
                            Nothing -> (Player.defaultPlayer |> Player.calculateHP, Level.Normal) --SHOULD NEVER HAPPEN
            in
            ( { model | player = Just new_player, player_status = Player.InRoom, room_entry_type = entry_type, random_seed = new_random_seed }
            , Cmd.none
            )
        Types.EnterRoom ->
            ( { model | player_status = Player.InRoom, current_room = model.current_room + 1 }
            , Cmd.none
            )
        Types.FinishRoom ->
            let
                new_status =
                    if (List.length model.current_level.rooms) == model.current_room then
                        Player.BetweenLevels
                    else
                        Player.BetweenRooms
            in
            ( { model | player_status = new_status }
            , Cmd.none
            )
        Types.FinishLevel ->
            let
                (new_level, status) =
                    case model.current_level.level of
                        1 -> (Level.level2, Player.BetweenLevels)
                        2 -> (Level.level3, Player.BetweenLevels)
                        3 -> (Level.level4, Player.BetweenLevels)
                        4 -> (model.current_level, Player.Finished)
                        _ -> (model.current_level, Player.Finished)
            in
            ( { model | player_status = status, current_level = new_level, current_room = 0 }
            , Cmd.none
            )
        Types.BetweenRoomRest room->
            let
                room_enemies = room.enemies
                new_player = 
                    case model.player of
                        Just p -> p |> Action.afterRoomEffect Action.Rest room_enemies model.random_seed
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
            in
            ( { model | player = Just new_player }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.BetweenRoomLoot room ->
            let
                room_enemies = room.enemies
                new_player = 
                    case model.player of
                        Just p -> p
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
                
                (updated_player, new_seed) = Action.afterRoomEffect Action.Loot room_enemies model.random_seed new_player
            in
            ( { model | player = Just updated_player, random_seed = new_seed }
            , Types.performMessage <| Types.EnterRoom
            )
        Types.BetweenRoomRush room ->
            let
                room_enemies = room.enemies
                new_player = 
                    case model.player of
                        Just p -> p |> Action.afterRoomEffect Action.Rush room_enemies model.random_seed
                        Nothing -> Player.baseRogue |> Player.calculateHP --SHOULD NEVER HAPPEN
            in
            ( { model | player = Just new_player }
            , Types.performMessage <| Types.EnterRoom
            )


        Types.ResetGame ->
            ( { model | game_started = False
                      , player = Nothing
                      , temp_player = Nothing
                      , class_picked = False
                      , points_to_spend = 10
                      , point_buy_complete = False
                      , player_status = Player.NotStarted
                      , current_level = Level.level1
                      , current_room = 0 }
            , Cmd.none
            )
        Types.JumpToFinish ->
            ( { model | player_status = Player.Finished }
            , Cmd.none
            )
        Types.GetTime ->
            ( model
            , Task.perform <| Types.GetCleanTime Time.now
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