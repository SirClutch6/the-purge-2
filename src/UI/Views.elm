module UI.Views exposing (..)

import Frontend.Model as FM
import Types
import UI.Components.Buttons as Btn

import Tailwind.Theme as TW
import Tailwind.Utilities as TW

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE

import Types.Player as Player
import Types.Levels as Level

viewGameNotStarted : FM.Model -> List (HS.Html Types.FrontendMsg)
viewGameNotStarted model =
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            , TW.items_center
            ]
        ]
        [ HS.div
                [ 
                ]
                [ HS.text "Welcome to my game!"
                ]
        , Btn.button Types.StartGame (Just "Start Game")
            |> Btn.toHtml
        ]
    ]

viewPickClass : FM.Model -> List (HS.Html Types.FrontendMsg)
viewPickClass model =
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            , TW.space_y_4
            , TW.items_center
            -- , TW.justify_center
            ]
        ]
        [ HS.text "Pick a class!"
        , HS.div
            [ HSA.css
                [ TW.space_x_10
                ]
            ]
            [ Btn.button Types.ChoseRogue (Just "Rogue")
                |> Btn.toHtml
            , Btn.button Types.ChoseSpy (Just "Spy")
                |> Btn.toHtml
            , Btn.button Types.ChoseWarrior (Just "Warrior")
                |> Btn.toHtml
            , Btn.button Types.ChoseTank (Just "Tank")
                |> Btn.toHtml
            ]
        ]
    ]    

viewPointBuy : FM.Model -> List (HS.Html Types.FrontendMsg)
viewPointBuy model =
    let
        temp = (Maybe.withDefault Player.defaultPlayer model.temp_player)
    in
    case model.player of
            Just player ->
                [ HS.div
                    [ HSA.css
                        [ TW.flex
                        , TW.flex_col
                        , TW.space_y_4
                        , TW.justify_items_center
                        , TW.items_center
                        , TW.w_full
                        ]
                    ]
                    [ HS.div
                        [
                        ]
                        [ HS.text <| "Character Class: " ++ Player.classToString player.class
                        ]
                    , HS.div
                        [
                        ]
                        [ HS.text <| "Character HP: " ++ (String.fromInt player.max_hp)
                        ]
                    , adjustAttributeDiv player Player.Dexterity model.points_to_spend temp.dexterity
                    , adjustAttributeDiv player Player.Strength model.points_to_spend temp.strength
                    , adjustAttributeDiv player Player.Charisma model.points_to_spend temp.charisma
                    , adjustAttributeDiv player Player.Constitution model.points_to_spend temp.constitution
                    , HS.div
                        [
                        ]
                        [ HS.text <| "You have " ++ (String.fromInt model.points_to_spend) ++ " points left to spend."
                        ]
                    , Btn.button Types.ConfirmPointsBuy (Just "Finish") --Proceed/continue to game
                        |> Btn.toHtml
                    ]
                ]
            Nothing ->
                -- Should not be able to get here
                [ HS.text "Error, no player found" ]

viewInGame : FM.Model -> List (HS.Html Types.FrontendMsg)
viewInGame model =
    -- Show Room, Between Room, Between Level, etc.
    let
        level = model.current_level
        current_room = 
            List.filter (\room -> room.num == model.current_room) level.rooms
                |> List.head
                |> Maybe.withDefault (Level.defaultRoom) -- Should not be able to get here
    in
    case model.player_status of
        Player.StartedEntry ->
            -- Approach building scene
            viewStartedEntry (Maybe.withDefault Player.defaultPlayer model.player)
        Player.InRoom ->
            -- Show room scene
            viewInRoom (Maybe.withDefault Player.defaultPlayer model.player) current_room
        Player.BetweenRooms ->
            -- Show between room scene
            viewBetweenRooms (Maybe.withDefault Player.defaultPlayer model.player) current_room
        Player.BetweenLevels ->
            -- Show between level scene
            viewBetweenLevels (Maybe.withDefault Player.defaultPlayer model.player) level
        Player.Finished ->
            -- Show finished scene
            viewFinished (Maybe.withDefault Player.defaultPlayer model.player)
        _ ->
            -- Should not be able to get here
            [ HS.text "Error, invalid player status" ]

viewStartedEntry : Player.Player -> List (HS.Html Types.FrontendMsg)
viewStartedEntry player = 
    [ HS.text "About to enter building" ]

viewInRoom : Player.Player -> Level.Room -> List (HS.Html Types.FrontendMsg)
viewInRoom player room =
    [ HS.text "In room" ]

viewBetweenRooms : Player.Player -> Level.Room -> List (HS.Html Types.FrontendMsg)
viewBetweenRooms player room =
    [ HS.text "Between rooms" ]

viewBetweenLevels : Player.Player -> Level.Level -> List (HS.Html Types.FrontendMsg)
viewBetweenLevels player level =
    [ HS.text "Between levels" ]

viewFinished : Player.Player -> List (HS.Html Types.FrontendMsg)
viewFinished player =
    [ HS.text "Finished" ]

adjustAttributeDiv : Player.Player -> Player.Attribute -> Int -> Int -> HS.Html Types.FrontendMsg
adjustAttributeDiv player attr points_remaining floor =
    let
        ceiling = 12
        (attr_name, attr_value) =
            case attr of
                Player.Dexterity ->
                    ("Dexterity", player.dexterity)
                Player.Strength ->
                    ("Strength", player.strength)
                Player.Charisma ->
                    ("Charisma", player.charisma)
                Player.Constitution ->
                    ("Constitution", player.constitution)
                
        btn_down =
            if attr_value <= floor then
                Btn.button Types.NoOpFrontendMsg (Just "-")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else
                Btn.button (Types.AdjustAttr attr -1) (Just "-")
                    |> Btn.toHtml

        btn_up =
            if points_remaining <= 0 then
                Btn.button Types.NoOpFrontendMsg (Just "+")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else if attr_value >= ceiling then
                Btn.button Types.NoOpFrontendMsg (Just "+")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else
                Btn.button (Types.AdjustAttr attr 1) (Just "+")
                    |> Btn.toHtml
    in
    HS.div
        [ HSA.css
            [ TW.space_x_2
            , TW.space_y_2
            ]
        ]
        [ HS.text <| attr_name 
        , btn_down
        , HS.text <| String.fromInt attr_value 
        , btn_up
        ]


