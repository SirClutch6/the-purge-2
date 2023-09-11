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

viewGameStarted : FM.Model -> List (HS.Html Types.FrontendMsg)
viewGameStarted model =
    let
        temp = 
            case model.temp_player of
                Just p -> p
                Nothing -> Player.baseRogue --Should not be able to get here
    in
    case model.class_picked of
        False ->
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
                , Btn.button Types.ChoseRogue (Just "Rogue")
                    |> Btn.toHtml
                , Btn.button Types.ChoseSpy (Just "Spy")
                    |> Btn.toHtml
                , Btn.button Types.ChoseWarrior (Just "Warrior")
                    |> Btn.toHtml
                , Btn.button Types.ChoseTank (Just "Tank")
                    |> Btn.toHtml
                ]
            ]
            
        True ->
            case model.player of
                Just player ->
                    [ HS.div
                        [ HSA.css
                            [ TW.flex
                            , TW.flex_col
                            , TW.space_y_4
                            , TW.justify_items_end
                            ]
                        ]
                        [ HS.text <| "Character Class: " ++ Player.classToString player.class
                        , HS.text <| "Character HP: " ++ (String.fromInt player.max_hp)
                        , adjustAttributeDiv player Player.Dexterity model.points_to_spend temp.dexterity
                        , adjustAttributeDiv player Player.Strength model.points_to_spend temp.strength
                        , adjustAttributeDiv player Player.Charisma model.points_to_spend temp.charisma
                        , adjustAttributeDiv player Player.Constitution model.points_to_spend temp.constitution
                        , HS.text <| "You have " ++ (String.fromInt model.points_to_spend) ++ " points left to spend."
                        ]
                    ]
                    
                    
                Nothing ->
                    -- Should not be able to get here
                    [ HS.text "Error, no player found" ]
                    

viewGameNotStarted : FM.Model -> List (HS.Html Types.FrontendMsg)
viewGameNotStarted model =
    [ HS.div
        [ HSA.css
            [ TW.flex
            ]
        ]
        [ HS.text "Welcome to my game!"
        , Btn.button Types.StartGame (Just "Start Game")
            |> Btn.toHtml
        -- , HS.button
        --     [ HSA.css
        --         [ TW.bg_color TW.blue_500
        --         , TW.text_color TW.white
        --         , TW.p_2
        --         , TW.m_2
        --         , TW.rounded
        --         , TW.h_12
        --         , TW.w_24
        --         ]
        --     , HSE.onClick  <| Types.StartGame
        --     ]
        --     [ HS.text "Start Game" ]
        ]
    ]

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


