module Frontend.Update exposing (update)

import Types
import Types.Player as Player
import Types.Levels exposing (Level)

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
        Types.EnterRoom ->
            ( { model | player_status = Player.InRoom, current_room = model.current_room + 1 }
            , Cmd.none
            )
        Types.FinishRoom ->
            let
                new_status =
                    if (List.length current_level.rooms) == current_room then
                        Player.BetweenLevel
                    else
                        Player.BetweenRoom
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
            in
            ( { model | player_status = status, current_level = new_level, current_room = 0 }
            , Cmd.none
            )
        Types.BetweenRoomRest ->
            -- TODO: Add rest logic
            ( { model | player_status = Player.BetweenRooms }, Cmd.none)
        Types.BetweenRoomLoot ->
            -- TODO: Add loot logic
            ( { model | player_status = Player.BetweenRooms }, Cmd.none)
        Types.BetweenRoomRush ->
            -- TODO: Add rush logic
            ( { model | player_status = Player.BetweenRooms }, Cmd.none)



        _ ->
            ( model
            , Cmd.none
            )