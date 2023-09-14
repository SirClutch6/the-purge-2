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
import Types.Actions as Actions
import Types.Enemy as Enemy
import Css exposing (move)

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
                [ HSA.css
                    [ TW.absolute
                    , TW.top_7
                    ]
                ]
                [ HS.text "THE PURGE"
                ]
        , HS.div
            [ HSA.css
                [ TW.absolute
                , TW.top_2over3
                ]
            ]
            [
                Btn.button Types.StartGame (Just "Start Game")
                    |> Btn.toHtml
            ]
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
                , TW.absolute
                , TW.top_2over3
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
                    , HS.div
                        [ HSA.css
                            [ TW.absolute
                            , TW.top_2over3
                            ]
                        ]
                        [
                            Btn.button Types.ConfirmPointsBuy (Just "Finish") --Proceed/continue to game
                                |> Btn.toHtml
                        ]
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
        hide_taunt = if Tuple.second model.enemy_taunted > 0 then True else False
        hide_furious_attack = if model.furious_attack_cooldown > 0 then True else False
        hide_stealth = if Tuple.second model.player_stealthed > 0 then True else False
        hide_self_heal = if model.self_heal_cooldown > 0 then True else False
    in
    case model.player_status of
        Player.StartedEntry ->
            -- Approach building scene
            viewStartedEntry (Maybe.withDefault Player.defaultPlayer model.player)
        Player.InRoom ->
            -- Show room scene
            viewInRoom (Maybe.withDefault Player.defaultPlayer model.player) current_room model.distance_from_enemy model.room_entry_type model.show_player_action_options hide_taunt hide_furious_attack hide_stealth hide_self_heal
        Player.BetweenRooms ->
            -- Show between room scene
            viewBetweenRooms (Maybe.withDefault Player.defaultPlayer model.player) current_room
        Player.BetweenLevels ->
            -- Show between level scene
            viewBetweenLevels (Maybe.withDefault Player.defaultPlayer model.player) level
        Player.Finished ->
            -- Show finished scene
            viewFinished (Maybe.withDefault Player.defaultPlayer model.player)
        Player.Lost ->
            -- Show lost scene
            viewLost
        _ ->
            -- Should not be able to get here
            [ HS.text "Error, invalid player status" ]

viewStartedEntry : Player.Player -> List (HS.Html Types.FrontendMsg)
viewStartedEntry _ = 
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            , TW.items_center
            ]
        ]
        [ HS.div
            [ HSA.css
                [ TW.absolute
                , TW.top_1over3
                ]
            ]
            [ textDivHelper "You hide outside the building."
            , textDivHelper "From your surveilance you believe there is only one guard posted in the entry room to the building."
            , textDivHelper "Would you like to enter through the doors, or search for another way in?" 
            ]            
        , HS.div
            [ HSA.css
                [ TW.absolute
                , TW.top_2over3
                , TW.space_x_2
                ]
            ]
            [ Btn.button Types.EnterBuilding (Just "Enter")
                |> Btn.toHtml
            , Btn.button Types.SearchForAnotherWayIn (Just "Search")
                |> Btn.toHtml
            ]
            ]
    ]

viewInRoom : Player.Player -> Level.Room -> Actions.Distance -> Level.RoomEntryType -> Bool -> Bool -> Bool -> Bool -> Bool -> List (HS.Html Types.FrontendMsg)
viewInRoom player room distance msg show_btns hide_taunt hide_furious_attack hide_stealth hide_self_heal =
    let
        occ = 
            if List.length room.enemies > 1 then
                "Occupants"
            else
                "Occupant"
        text = 
            case msg of
                Level.DexSneak ->
                    "You have successfully found another way into the building.\n You get two free actions against the unsuspecting " ++ occ ++ "."
                Level.ChrSneak ->
                    "You have found another way into the building, but you were spotted by a guard. Luckily you were able to come up with a good excuse to be there and he lets you go."    
                Level.FailedSneak ->
                    "While looking for another way in you take a nasty fall and lose 5 hp."
                Level.Rush ->
                    "You quickly enter the room, catching its " ++ occ ++ " off guard."
                Level.Normal ->
                    "You proceed into the room."
        (attack_btn, move_btn) = 
            if distance == Actions.Range then
                ( Btn.button (Types.PlayerAttack Actions.Range 1) (Just "Ranged Attack")
                    |> Btn.toHtml
                , Btn.button (Types.PlayerMove Actions.Toward) (Just "Approach Guard")
                    |> Btn.toHtml
                )
            else 
                ( Btn.button (Types.PlayerAttack Actions.Melee 1) (Just "Melee Attack")
                    |> Btn.toHtml
                , Btn.button (Types.PlayerMove Actions.Away) (Just "Push Guard Away")
                    |> Btn.toHtml
                )
        taunt_button =
            if hide_taunt then
                Btn.button (Types.PlayerTaunt) (Just "Taunt")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else
                Btn.button (Types.PlayerTaunt) (Just "Taunt")
                    |> Btn.toHtml 
        furious_attack_button =
            if hide_furious_attack then
                Btn.button (Types.PlayerFuriousAttack) (Just "Furious Attack")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else
                Btn.button (Types.PlayerFuriousAttack) (Just "Furious Attack")
                    |> Btn.toHtml
        stealth_button =
            if hide_stealth then
                Btn.button (Types.PlayerStealth) (Just "Enter Stealth")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else
                Btn.button (Types.PlayerStealth) (Just "Enter Stealth")
                    |> Btn.toHtml
        self_heal_button =
            if hide_self_heal then
                Btn.button (Types.PlayerHeal) (Just "Heal")
                    |> Btn.withDisabled
                    |> Btn.toHtml
            else
                Btn.button (Types.PlayerHeal) (Just "Heal")
                    |> Btn.toHtml
        action_buttons = 
            if show_btns then
                [ move_btn
                , attack_btn
                , taunt_button
                , furious_attack_button
                , stealth_button
                , self_heal_button
                ]
            else
                []
    in
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            , TW.items_center
            ]
        ]
        ([ HS.text text
        , HS.div
            [ HSA.css
                [ TW.space_x_2
                , TW.absolute
                , TW.top_2over3
                ]
            ]
            action_buttons
        -- , Btn.button Types.JumpToFinish (Just "Skip to Finish")
        --     |> Btn.toHtml
        ] ++ viewEnemyHelper room)
    ]
    

viewBetweenRooms : Player.Player -> Level.Room -> List (HS.Html Types.FrontendMsg)
viewBetweenRooms player room =
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            ]
        ]
        [ HS.div
        [ HSA.css
            [ 
            ]
        ]
        [ HS.text "You have completed the room."
        , HS.text "What will you do between rooms?"
        , HS.text ("HP: " ++ (String.fromInt player.hp) ++ "/" ++ (String.fromInt player.max_hp) ++ "\n")
        , HS.text ("Sanity%: " ++ (String.fromInt player.sanity) ++ "\n")
        ]
        , HS.div
            [ HSA.css
                [ TW.space_x_10
                ]
            ]
            [ Btn.button (Types.BetweenRoomRest room) (Just "Rest")
                |> Btn.toHtml
            , Btn.button (Types.BetweenRoomLoot room) (Just "Loot")
                |> Btn.toHtml
            , Btn.button (Types.BetweenRoomRush room) (Just "Rush")
                |> Btn.toHtml
            ]
        ]
    ]
    
    

viewBetweenLevels : Player.Player -> Level.Level -> List (HS.Html Types.FrontendMsg)
viewBetweenLevels player level =
    [ HS.text "Between levels" ] --TODO add this functionality

viewFinished : Player.Player -> List (HS.Html Types.FrontendMsg)
viewFinished _ =   
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            ]
        ]
        [ HS.text "As The Captain falls to the ground, his final taunt floats to your ears.\n" 
        , HS.text "\"Please, please don't do this...\"\n"
        , HS.text "Finally, the last of the police have been eliminated. The people of the city are free; free from laws, restrictions, and limits.\n"
        , HS.text "Let The Purge begin..."
        , Btn.button Types.ResetGame (Just "Reset Game")
            |> Btn.toHtml
        ]
    ]

viewLost : List (HS.Html Types.FrontendMsg)
viewLost =
    [ HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            , TW.items_center
            ]
        ]
        [ HS.text "You have failed. The city remains under tyranny." 
        , HS.div
            [ HSA.css
                [ TW.absolute
                , TW.top_2over3
                ]
            ] 
            [ Btn.button Types.ResetGame (Just "Start Over")
                |> Btn.toHtml
            ]
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

eventLogDiv : String -> HS.Html Types.FrontendMsg
eventLogDiv string =
    HS.div
        [ HSA.css
            [ 
            ]
        ]
        [ HS.text string
        ]

viewPlayerHelper : FM.Model -> List (HS.Html Types.FrontendMsg)
viewPlayerHelper model =
    let
        player =
            case model.player of
                Just p ->
                    p
                Nothing ->
                    Player.defaultPlayer
        player_taunt_cooldown_view =
            let
                cooldown = Tuple.second model.enemy_taunted
            in
            if cooldown > 0 then
                [ HS.div
                    [ HSA.css
                        [ TW.flex
                        , TW.flex_row
                        , TW.space_x_2
                        ]
                    ]
                    [ HS.text <| "Player Taunt Cooldown: "
                    , HS.text <| String.fromInt cooldown
                    ]
                ]
            else
                []
        furious_attack_cooldown_view =
            if model.furious_attack_cooldown > 0 then
                [ HS.div
                    [ HSA.css
                        [ TW.flex
                        , TW.flex_row
                        , TW.space_x_2
                        ]
                    ]
                    [ HS.text <| "Furious Attack Cooldown: "
                    , HS.text <| String.fromInt model.furious_attack_cooldown
                    ]
                ]
            else
                []
        player_stealth_cooldown_view =
            let
                cooldown = Tuple.second model.player_stealthed
            in
            if cooldown > 0 then
                [ HS.div
                    [ HSA.css
                        [ TW.flex
                        , TW.flex_row
                        , TW.space_x_2
                        ]
                    ]
                    [ HS.text <| "Player Stealth Cooldown: "
                    , HS.text <| String.fromInt cooldown
                    ]
                ]
            else
                []
        self_heal_cooldown_view =
            if model.self_heal_cooldown > 0 then
                [ HS.div
                    [ HSA.css
                        [ TW.flex
                        , TW.flex_row
                        , TW.space_x_2
                        ]
                    ]
                    [ HS.text <| "Self Heal Cooldown: "
                    , HS.text <| String.fromInt model.self_heal_cooldown
                    ]
                ]
            else
                []
        stats = 
            if model.point_buy_complete then
                [ viewPlayerStatsHelper player "HP"
                , viewPlayerStatsHelper player "Sanity"
                , viewPlayerStatsHelper player "Dexterity"
                , viewPlayerStatsHelper player "Strength"
                , viewPlayerStatsHelper player "Charisma"
                , viewPlayerStatsHelper player "Constitution"
                , viewPlayerStatsHelper player "Rush"
                , viewPlayerStatsHelper player "Coins"
                , viewPlayerStatsHelper player "Turn Initiative"
                ] ++ player_taunt_cooldown_view ++ furious_attack_cooldown_view ++ player_stealth_cooldown_view ++ self_heal_cooldown_view
            else
                []
    in
    [ HS.div
        [ HSA.css
            [ TW.absolute
            , TW.top_4
            , TW.flex
            , TW.flex_col
            , TW.top_0
            , TW.left_1over4
            , TW.m_2
            ]
        ]
        [ HS.div
            [ HSA.css
                [ TW.underline
                , TW.flex
                , TW.flex_row
                , TW.space_x_2
                ]
            ]
            [ HS.text <| "Player" ]
        , HS.div
            [ 
            ]
            ([ HS.div   
                [ HSA.css
                    [ TW.flex
                    , TW.flex_row
                    , TW.space_x_2
                    ]
                ]
                [ HS.text <| "Class: "
                , HS.text <| Player.classToString player.class
                ]
            ] ++ stats)
        ]
    ]

viewPlayerStatsHelper : Player.Player -> String -> HS.Html Types.FrontendMsg
viewPlayerStatsHelper player stat =
    let
        display =
            if stat == "HP" then
                HS.text <| (String.fromInt <| Player.getStat stat player) ++ "/" ++ (String.fromInt <| Player.getStat "Max HP" player)
            else if stat == "Sanity" then
                HS.text <| (String.fromInt <| Player.getStat stat player) ++ "%"
            else
                HS.text <| String.fromInt <| Player.getStat stat player
    in
    HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_row
            , TW.space_x_2
            ]
        ]
        [ HS.text <| stat ++ ": "
        , display
        ]

viewEnemyHelper : Level.Room -> List (HS.Html Types.FrontendMsg)
viewEnemyHelper room =
    let
        enemies = room.enemies
    in
    [ HS.div
        [ HSA.css
            [ TW.absolute
            , TW.top_1over3
            -- , TW.flex
            -- , TW.flex_col
            -- , TW.top_0
            -- , TW.left_1over4
            , TW.m_2
            , TW.space_x_5
            ]
        ]
        ( List.map (\enemy -> viewEachEnemyHelper enemy) enemies )
    ]
    


viewEachEnemyHelper : Enemy.Enemy -> HS.Html Types.FrontendMsg
viewEachEnemyHelper enemy =
    let
        stats = 
            [ viewEnemyStatsHelper enemy "HP"
            , viewEnemyStatsHelper enemy "Rush"
            , viewEnemyStatsHelper enemy "Turn Initiative"
            ]
    in
    HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_col
            ]
        ]
        [ HS.div
            [ HSA.css
                [ TW.underline
                , TW.flex
                , TW.flex_row
                , TW.space_x_2
                ]
            ]
            [ HS.text <| ("Enemy" ++ String.fromInt enemy.id) ]
        , HS.div
            [ 
            ]
            ([ HS.div   
                [ HSA.css
                    [ TW.flex
                    , TW.flex_row
                    , TW.space_x_2
                    ]
                ]
                [ HS.text <| "Class: "
                , HS.text <| Enemy.classToString enemy.class
                ]
            ] ++ stats)
        ]

viewEnemyStatsHelper : Enemy.Enemy -> String -> HS.Html Types.FrontendMsg
viewEnemyStatsHelper enemy stat =
    let
        display =
            if stat == "HP" then
                HS.text <| (String.fromInt <| Enemy.getStat stat enemy) ++ "/" ++ (String.fromInt <| Enemy.getStat "Max HP" enemy)
            else
                HS.text <| String.fromInt <| Enemy.getStat stat enemy
    in
    HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_row
            , TW.space_x_2
            ]
        ]
        [ HS.text <| stat ++ ": "
        , display
        ]


textDivHelper : String -> HS.Html Types.FrontendMsg
textDivHelper string =
    HS.div
        [ HSA.css
            [ TW.flex
            , TW.flex_row
            , TW.space_x_2
            ]
        ]
        [ HS.text string
        ]