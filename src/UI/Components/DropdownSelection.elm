module UI.Components.DropdownSelection exposing (..)

import Html.Styled as HS
import Html.Styled.Attributes as HSA

import Tailwind.Theme as TW
import Tailwind.Utilities as TW
import Tailwind.Breakpoints as TWB

import Frontend.Model as FM

import Types as Types

import UI.Components.Helpers as Help



-- type Radio msg option
--     = Radio Options (option -> msg) (List (HS.option option msg)) (Maybe option) String

type DropdownSelection msg
    = DropdownSelection Options (List (DropdownItem msg))


-- type Label
--     = LabelHidden
--     | LabelAbove
--     | LabelBelow
--     | LabelRight
--     | LabelLeft

type alias DropdownItem msg =
    { display : HS.Html msg
    }


type alias Options =
    { size : String
    }


defaultOptions : Options
defaultOptions =
    { size = "fit"
    }


-- radio : (option -> msg) -> List (EI.Option option msg) -> Maybe option -> String -> Radio msg option
-- radio msg option_list m_selected label_text =
--     Radio defaultOptions msg option_list m_selected label_text

dropdownSelection : List (DropdownItem msg) -> DropdownSelection msg
dropdownSelection list =
    DropdownSelection defaultOptions list


-- toElement : Radio msg option -> E.Element msg
-- toElement (Radio options msg option_list m_selected label_text) =
toHtml : DropdownSelection msg -> HS.Html msg
toHtml (DropdownSelection options list) =
    let
        size = Help.formatWidth options.size
    in
    HS.select
        [ HSA.css
            [ size
            ]
        ]
        (List.map
            (\list_item -> dropdownItem list_item)   
            list 
        )

dropdownItem : DropdownItem msg -> HS.Html msg
dropdownItem item =
    HS.option
        []
        [ item.display
        ]

withSize : String -> DropdownSelection msg -> DropdownSelection msg
withSize size (DropdownSelection options list) =
    DropdownSelection { options | size = size } list
    -- let
    --     label_style =
    --         [ E.paddingXY 0 4
    --         , EF.size UI.fontSize.text.lg
    --         , EF.color UI.colorPalette.dark_grey
    --         ]

    --     label =
    --         case options.label of
    --             LabelHidden ->
    --                 EI.labelHidden label_text

    --             LabelAbove ->
    --                 EI.labelAbove label_style (E.text label_text)

    --             LabelBelow ->
    --                 EI.labelBelow label_style (E.text label_text)

    --             LabelRight ->
    --                 EI.labelRight label_style (E.text label_text)

    --             LabelLeft ->
    --                 EI.labelLeft label_style (E.text label_text)
    -- in
    -- if options.in_row then
    --     EI.radioRow
    --         [ E.spacing 12
    --         , EF.size UI.fontSize.text.sm
    --         , EF.color UI.colorPalette.dark_grey
    --         ]
    --         { onChange = msg
    --         , options = option_list
    --         , selected = m_selected
    --         , label = label
    --         }

    -- else
    --     EI.radio
    --         [ E.spacing 6
    --         , EF.size UI.fontSize.text.sm
    --         , EF.color UI.colorPalette.dark_grey
    --         ]
    --         { onChange = msg
    --         , options = option_list
    --         , selected = m_selected
    --         , label = label
    --         }


-- withInRow : Radio msg option -> Radio msg option
-- withInRow (Radio options msg option_list m_selected label_text) =
--     Radio { options | in_row = True } msg option_list m_selected label_text


-- withLabelLocation : Label -> Radio msg option -> Radio msg option
-- withLabelLocation label (Radio options msg option_list m_selected label_text) =
--     Radio { options | label = label } msg option_list m_selected label_text