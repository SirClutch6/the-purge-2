module UI.Components.Helpers exposing (..)

import Tailwind.Theme as TW
import Tailwind.Utilities as TW
import Tailwind.Breakpoints as TWB

import Html.Styled.Attributes as HSA
import Css exposing (Style)

type PossibleColor
    = Inherit
    | Current
    | Transparent
    | Black
    | White
    | Slate_50
    | Slate_100
    | Slate_200
    | Slate_300
    | Slate_400
    | Slate_500
    | Slate_600
    | Slate_700
    | Slate_800
    | Slate_900
    | Slate_950
    | Gray_50
    | Gray_100
    | Gray_200
    | Gray_300
    | Gray_400
    | Gray_500
    | Gray_600
    | Gray_700
    | Gray_800
    | Gray_900
    | Gray_950
    | Zinc_50
    | Zinc_100
    | Zinc_200
    | Zinc_300
    | Zinc_400
    | Zinc_500
    | Zinc_600
    | Zinc_700
    | Zinc_800
    | Zinc_900
    | Zinc_950
    | Neutral_50
    | Neutral_100
    | Neutral_200
    | Neutral_300
    | Neutral_400
    | Neutral_500
    | Neutral_600
    | Neutral_700
    | Neutral_800
    | Neutral_900
    | Neutral_950
    | Stone_50
    | Stone_100
    | Stone_200
    | Stone_300
    | Stone_400
    | Stone_500
    | Stone_600
    | Stone_700
    | Stone_800
    | Stone_900
    | Stone_950
    | Red_50
    | Red_100
    | Red_200
    | Red_300
    | Red_400
    | Red_500
    | Red_600
    | Red_700
    | Red_800
    | Red_900
    | Red_950
    | Orange_50
    | Orange_100
    | Orange_200
    | Orange_300
    | Orange_400
    | Orange_500
    | Orange_600
    | Orange_700
    | Orange_800
    | Orange_900
    | Orange_950
    | Amber_50
    | Amber_100
    | Amber_200
    | Amber_300
    | Amber_400
    | Amber_500
    | Amber_600
    | Amber_700
    | Amber_800
    | Amber_900
    | Amber_950
    | Yellow_50
    | Yellow_100
    | Yellow_200
    | Yellow_300
    | Yellow_400
    | Yellow_500
    | Yellow_600
    | Yellow_700
    | Yellow_800
    | Yellow_900
    | Yellow_950
    | Lime_50
    | Lime_100
    | Lime_200
    | Lime_300
    | Lime_400
    | Lime_500
    | Lime_600
    | Lime_700
    | Lime_800
    | Lime_900
    | Lime_950
    | Green_50
    | Green_100
    | Green_200
    | Green_300
    | Green_400
    | Green_500
    | Green_600
    | Green_700
    | Green_800
    | Green_900
    | Green_950
    | Emerald_50
    | Emerald_100
    | Emerald_200
    | Emerald_300
    | Emerald_400
    | Emerald_500
    | Emerald_600
    | Emerald_700
    | Emerald_800
    | Emerald_900
    | Emerald_950
    | Teal_50
    | Teal_100
    | Teal_200
    | Teal_300
    | Teal_400
    | Teal_500
    | Teal_600
    | Teal_700
    | Teal_800
    | Teal_900
    | Teal_950
    | Cyan_50
    | Cyan_100
    | Cyan_200
    | Cyan_300
    | Cyan_400
    | Cyan_500
    | Cyan_600
    | Cyan_700
    | Cyan_800
    | Cyan_900
    | Cyan_950
    | Sky_50
    | Sky_100
    | Sky_200
    | Sky_300
    | Sky_400
    | Sky_500
    | Sky_600
    | Sky_700
    | Sky_800
    | Sky_900
    | Sky_950
    | Blue_50
    | Blue_100
    | Blue_200
    | Blue_300
    | Blue_400
    | Blue_500
    | Blue_600
    | Blue_700
    | Blue_800
    | Blue_900
    | Blue_950
    | Indigo_50
    | Indigo_100
    | Indigo_200
    | Indigo_300
    | Indigo_400
    | Indigo_500
    | Indigo_600
    | Indigo_700
    | Indigo_800
    | Indigo_900
    | Indigo_950
    | Violet_50
    | Violet_100
    | Violet_200
    | Violet_300
    | Violet_400
    | Violet_500
    | Violet_600
    | Violet_700
    | Violet_800
    | Violet_900
    | Violet_950
    | Purple_50
    | Purple_100
    | Purple_200
    | Purple_300
    | Purple_400
    | Purple_500
    | Purple_600
    | Purple_700
    | Purple_800
    | Purple_900
    | Purple_950
    | Fuchsia_50
    | Fuchsia_100
    | Fuchsia_200
    | Fuchsia_300
    | Fuchsia_400
    | Fuchsia_500
    | Fuchsia_600
    | Fuchsia_700
    | Fuchsia_800
    | Fuchsia_900
    | Fuchsia_950
    | Pink_50
    | Pink_100
    | Pink_200
    | Pink_300
    | Pink_400
    | Pink_500
    | Pink_600
    | Pink_700
    | Pink_800
    | Pink_900
    | Pink_950
    | Rose_50
    | Rose_100
    | Rose_200
    | Rose_300
    | Rose_400
    | Rose_500
    | Rose_600
    | Rose_700
    | Rose_800
    | Rose_900
    | Rose_950


formatPadding : Int -> Style
formatPadding num =
    case num of
        0 ->
            TW.p_0
        1 ->
            TW.p_1
        2 ->
            TW.p_2
        3 ->
            TW.p_3
        4 ->
            TW.p_4
        5 ->
            TW.p_5
        6 ->
            TW.p_6
        7 ->
            TW.p_7
        8 ->
            TW.p_8
        9 ->
            TW.p_9
        10 ->
            TW.p_10
        11 ->
            TW.p_11
        12 ->
            TW.p_12
        14 ->
            TW.p_14
        16 ->
            TW.p_16
        20 ->
            TW.p_20
        24 ->
            TW.p_24
        28 ->
            TW.p_28
        32 ->
            TW.p_32
        36 ->
            TW.p_36
        40 ->
            TW.p_40
        44 ->
            TW.p_44
        48 ->
            TW.p_48
        52 ->
            TW.p_52
        56 ->
            TW.p_56
        60 ->
            TW.p_60
        64 ->
            TW.p_64
        72 ->
            TW.p_72
        80 ->
            TW.p_80
        96 ->
            TW.p_96
        _ ->
            TW.p_0

formatWidth : String -> Style
formatWidth width =
    case width of
        "0" ->
            TW.w_0
        "1" ->
            TW.w_1
        "2" ->
            TW.w_2
        "3" ->
            TW.w_3
        "4" ->
            TW.w_4
        "5" ->
            TW.w_5
        "6" ->
            TW.w_6
        "7" ->
            TW.w_7
        "8" ->
            TW.w_8
        "9" ->
            TW.w_9
        "10" ->
            TW.w_10
        "11" ->
            TW.w_11
        "12" ->
            TW.w_12
        "14" ->
            TW.w_14
        "16" ->
            TW.w_16
        "20" ->
            TW.w_20
        "24" ->
            TW.w_24
        "28" ->
            TW.w_28
        "32" ->
            TW.w_32
        "36" ->
            TW.w_36
        "40" ->
            TW.w_40
        "44" ->
            TW.w_44
        "48" ->
            TW.w_48
        "52" ->
            TW.w_52
        "56" ->
            TW.w_56
        "60" ->
            TW.w_60
        "64" ->
            TW.w_64
        "72" ->
            TW.w_72
        "80" ->
            TW.w_80
        "96" ->
            TW.w_96
        "auto" ->
            TW.w_auto
        "full" ->
            TW.w_full
        "screen" ->
            TW.w_screen
        "min" ->
            TW.w_min
        "max" ->
            TW.w_max
        "fit" ->
            TW.w_fit
        _ ->
            TW.w_fit

formatFontSize : String -> Style
formatFontSize size =
    case size of
        "xs" ->
            TW.text_xs
        "sm" ->
            TW.text_sm
        "base" ->
            TW.text_base
        "lg" ->
            TW.text_lg
        "xl" ->
            TW.text_xl
        "2xl" ->
            TW.text_2xl
        "3xl" ->
            TW.text_3xl
        "4xl" ->
            TW.text_4xl
        "5xl" ->
            TW.text_5xl
        "6xl" ->
            TW.text_6xl
        "7xl" ->
            TW.text_7xl
        "8xl" ->
            TW.text_8xl
        "9xl" ->
            TW.text_9xl
        _ ->
            TW.text_base

formatTextAlign : String -> Style
formatTextAlign align =
    case align of
        "left" ->
            TW.text_left
        "center" ->
            TW.text_center
        "right" ->
            TW.text_right
        "justify" ->
            TW.text_justify
        "start" ->
            TW.text_start
        "end" ->
            TW.text_end
        _ ->
            TW.text_center

formatFontWeight : String -> Style
formatFontWeight weight =
    case weight of
        "thin" ->
            TW.font_thin
        "extralight" ->
            TW.font_extralight
        "light" ->
            TW.font_light
        "normal" ->
            TW.font_normal
        "medium" ->
            TW.font_medium
        "semibold" ->
            TW.font_semibold
        "bold" ->
            TW.font_bold
        "extrabold" ->
            TW.font_extrabold
        "black" ->
            TW.font_black
        _ ->
            TW.font_normal
        