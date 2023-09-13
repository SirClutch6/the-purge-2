module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Lamdera
import Types exposing (..)
import Url

import Frontend.Model as FM
import Frontend.Update as FU
import Frontend.UpdateFromBackend as FUB

import UI.Views as UI

import UI.Components.Buttons as Btn

import Tailwind.Theme as TW
import Tailwind.Utilities as TW

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import Task
import Time

-- import Element as E
-- import Element.Background as EBa
-- import Element.Border as EBo
-- import Element.Events as EEv
-- import Element.Font as EFo



-- type alias Model =
--     FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = FU.update
        , updateFromBackend = FUB.updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FM.Model, Cmd FrontendMsg )
init url key =
    let
        model =
            FM.defineModel <|
                FM.Init
                    { key = key
                    , url = url
                    }
    in
    ( model
    , Task.perform Types.GetCleanTime Time.now
    )


-- update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
-- update msg model =
--     case msg of
--         UrlClicked urlRequest ->
--             case urlRequest of
--                 Internal url ->
--                     ( model
--                     , Nav.pushUrl model.key (Url.toString url)
--                     )

--                 External url ->
--                     ( model
--                     , Nav.load url
--                     )

--         UrlChanged url ->
--             ( model, Cmd.none )

--         NoOpFrontendMsg ->
--             ( model, Cmd.none )


-- updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
-- updateFromBackend msg model =
--     case msg of
--         NoOpToFrontend ->
--             ( model, Cmd.none )


view : FM.Model -> Browser.Document Types.FrontendMsg
view model =
    let
        help_menu =
            if model.show_help_menu then
                HS.div
                [ HSA.css
                    [ TW.bg_color TW.red_100
                    , TW.h_3over4
                    , TW.w_1over4
                    , TW.absolute 
                    , TW.right_0
                    ]
                ]
                [ HS.text "HELP" -- TODO add help info here
                ]
            else
                HS.text ""
        log = List.map UI.eventLogDiv model.event_log
        event_log =
            HS.div
            [ HSA.css
                [ TW.bg_color TW.gray_200
                , TW.h_3over4
                , TW.w_1over4
                , TW.absolute 
                , TW.left_0
                ]
            ]
            [ HS.div
                [ HSA.css
                    [ TW.underline
                    ]
                ]
                [ HS.text "EVENT LOG" ]
            , HS.div
                [ HSA.css
                    [ TW.left_0
                    -- , TW.flex
                    -- , TW.flex_col
                    ]
                ]
                log
            ]
    in
    { title = ""
    , body =
        [ HS.toUnstyled <|
            HS.div
            [ HSA.css
                [ TW.flex
                , TW.justify_center 
                , TW.h_screen
                , TW.w_screen
                , TW.box_border
                , TW.sticky
                ]
            ]
            [ event_log
            , HS.div
                [ HSA.css
                    [ TW.bg_color TW.blue_200
                    , TW.h_3over4
                    , TW.w_2over4
                    , TW.justify_center
                    , TW.flex
                    , TW.text_center
                    , TW.box_border
                    , TW.p_4
                    ]
                ]
                (viewGame model)
                -- [ HS.text "Hello World" 
                -- ]
            , help_menu
            ]
        ]
    }

viewGame : FM.Model -> List (HS.Html Types.FrontendMsg)
viewGame model =
    let
        whatToView = 
            if not model.game_started then
                UI.viewGameNotStarted model
            else if not model.class_picked then
                UI.viewPickClass model
            else if not model.point_buy_complete then
                UI.viewPointBuy model
            else
                UI.viewInGame model
        -- reset_button =
        --     [ HS.div
        --         [ HSA.css
        --             [ TW.absolute
        --             , TW.top_4
        --             -- , TW.inset_x_12
        --             , TW.top_0
        --             , TW.left_1over4
        --             , TW.m_2
        --             ]
        --         ]
        --         [ Btn.button Types.ResetGame (Just "Reset Game")
        --             |> Btn.toHtml
        --         ]
        --     ]
        player_info =
            if model.class_picked then
                UI.viewPlayerHelper model
            else
                []
        help_button =
            [ HS.div
                [ HSA.css
                    [ TW.absolute
                    , TW.top_4
                    -- , TW.inset_x_12
                    , TW.right_1over4
                    , TW.top_0
                    , TW.m_2
                    ]
                ]
                [ Btn.button Types.ShowHelp (Just "?")
                    |> Btn.toHtml
                ]
            ]
    in
    player_info ++ whatToView ++ help_button
    
