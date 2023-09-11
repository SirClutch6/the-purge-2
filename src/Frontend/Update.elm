module Frontend.Update exposing (update)

import Types
import Types.Player as Player

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
                            { player | constitution = player.constitution + amount }
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
        _ ->
            ( model
            , Cmd.none
            )