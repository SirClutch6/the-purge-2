module UI.Components.Buttons exposing (..)

import Tailwind.Theme as TW
import Tailwind.Utilities as TW
import Tailwind.Breakpoints as TWB
-- import Tailwind.Color as TC

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import Frontend.Model as M

import UI.Components.Helpers as Help
-- import Components.UI as CUI

import Css


type Icon
    = Checkmark
    | Cross
    | Plus
    | House
    | People
    | Taskbox
    | ChevronRight
    | ChevronLeft
    | ChevronUp
    | ChevronDown


type Width
    = Exact Int
    | Fill
    | Shrink


type Color
    = ColorWhite
    | ColorBlack
    | ColorLightPlum
    | ColorPlum
    | ColorRed
    | ColorGrey
    | ColorDisabled
    | ColorNone


type Alignment
    = TopAlign
    | RightAlign
    | BottomAlign
    | LeftAlign
    | CenterX
    | CenterY


type Button msg
    = Button Options msg (Maybe String)

type alias Options =
    { icon : Maybe Icon
    , disabled : Bool
    , width : String
    , color : TW.Color
    , border_color : TW.Color
    , text_color : TW.Color
    , alignment : String
    , font_size : String
    , font_weight : String
    , padding : Int
    }

defaultOptions : Options
defaultOptions =
    { icon = Nothing
    , disabled = False
    , width = "fit"
    , color = TW.gray_300
    , border_color = TW.black
    , text_color = TW.black
    , alignment = "center"
    , font_size = "base"
    , font_weight = "normal"
    , padding = 5
    }

button : msg -> Maybe String -> Button msg
button msg m_label =
    Button defaultOptions msg m_label

toHtml : Button msg -> HS.Html msg
toHtml (Button options msg m_label) =
    let
        padding = Help.formatPadding options.padding
        width = Help.formatWidth options.width
        alignment = Help.formatTextAlign options.alignment
        font_size = Help.formatFontSize options.font_size
        font_weight = Help.formatFontWeight options.font_weight
        -- bg_color = Help.formatBackgroundColor options.color
        -- bg_color = TW.bg_color options.color
        -- border_color = TW.border_color options.border_color
        -- border = TW.border_2
        -- border_style = TW.border_solid
        hover = Css.hover [(Css.cursor Css.pointer)]
        attributes =
            HSA.css
                [ width
                , TW.h_12
                , TW.items_center
                , TW.justify_center
                , TW.rounded_md
                , alignment
                , font_size
                , font_weight
                , hover
                , TW.border_solid
                , TW.border_2
                , TW.border_color options.border_color
                , TW.bg_color options.color
                , TW.text_color options.text_color
                ]

        label =
            case m_label of
                Just x ->
                    x
                Nothing ->
                    "Missing Label"
                    -- Icon here
    in
    if options.disabled == False then
        HS.button
            [ attributes
            , HSE.onClick msg
            ]
            [ HS.text label
            ]
    else
        HS.div
            [
            ]
            [
            ]


-- ANCHOR Adjustments

{-| Adds an alignment. This can be combined; for example for a bottom right alignement:

    button NoOp Nothing
        |> withAlignment BottomAlign
        |> withALignment RightAlign
        |> toElement

-}
withAlignment : String -> Button msg -> Button msg
withAlignment alignment (Button options msg label) =
    Button { options | alignment = alignment } msg label


{-| Adds a specific color to the button.

    button NoOp Nothing
        |> withColor ColorRed
        |> toElement

-}
withColor : TW.Color -> Button msg -> Button msg
withColor color (Button options msg label) =
    Button { options | color = color } msg label

withBorderColor : TW.Color -> Button msg -> Button msg
withBorderColor color (Button options msg label) =
    Button { options | border_color = color } msg label

withTextColor : TW.Color -> Button msg -> Button msg
withTextColor color (Button options msg label) =
    Button { options | text_color = color } msg label

{-| Used to disable a button.

    button NoOp Nothing
        |> withDisabled
        |> toElement

-}
withDisabled : Button msg -> Button msg
withDisabled (Button options msg label) =
    Button { options | disabled = True } msg label


{-| Used to set the font size of a button.

    button NoOp Nothing
        |> withFontSize UI.fontSize.text.md
        |> toElement

-}
withFontSize : String -> Button msg -> Button msg
withFontSize font_size (Button options msg label) =
    Button { options | font_size = font_size } msg label

withFontWeight : String -> Button msg -> Button msg
withFontWeight font_weight (Button options msg label) =
    Button { options | font_weight = font_weight } msg label


{-| Add an Icon to the button
-}
withIcon : Icon -> Button msg -> Button msg
withIcon icon (Button options msg label) =
    Button { options | icon = Just icon } msg label


{-| Set a specific padding to the button.

    button NoOp Nothing
        |> withPadding 4
        |> toElement

-}
withPadding : Int -> Button msg -> Button msg
withPadding padding (Button options msg label) =
    Button { options | padding = padding } msg label


{-| Defines the width of the button.

    button NoOp Nothing
        |> withWidth Fill
        |> toElement

-}
withWidth : String -> Button msg -> Button msg
withWidth width (Button options msg label) =
    Button { options | width = width } msg label