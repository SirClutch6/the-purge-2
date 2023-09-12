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

-- import Backend.Update as BU

import Tailwind.Theme as TW
import Tailwind.Utilities as TW

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import Task

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
    , Types.performMessage <| Types.GetTime
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
    { title = ""
    , body =
        -- [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
        --     [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
        --     , Html.div
        --         [ Attr.style "font-family" "sans-serif"
        --         , Attr.style "padding-top" "40px"
        --         ]
        --         [ Html.text model.message ]
        --     ]
        -- ]
        [ HS.toUnstyled <|
            HS.div
            [ HSA.css
                [ TW.flex
                , TW.justify_center --Horizontal
                -- , TW.items_center --Vertical
                , TW.h_screen
                , TW.w_screen
                , TW.box_border
                ]
            ]
            [
                HS.div
                    [ HSA.css
                        [ TW.bg_color TW.blue_200
                        , TW.h_3over4
                        , TW.w_3over4
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
    in
    whatToView