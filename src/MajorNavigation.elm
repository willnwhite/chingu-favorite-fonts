module MajorNavigation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


wideOrNarrow windowWidth searchInput searchInputMsg searchMsg sampleTextInput sampleTextMsg fontSize fontSizeMsg resetMsg =
    if windowWidth >= (300 * 2 + 31) then
        -- hand-tuned to match Fonts.view
        wide searchInput searchInputMsg searchMsg sampleTextInput sampleTextMsg fontSize fontSizeMsg resetMsg

    else
        narrow searchInput searchInputMsg searchMsg resetMsg


wide searchInput searchInputMsg searchMsg sampleTextInput sampleTextMsg fontSize fontSizeMsg resetMsg =
    [ div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        , style "margin" "1.5em"
        , style "padding" "5px 10px"
        , style "border" "thin solid black"
        , style "border-radius" "48px"
        ]
        [ searchField searchInput searchInputMsg searchMsg
        , div
            [ style "width" "10px"
            , style "height" "20px"
            , style "border-right" "thin solid black"
            ]
            []

        -- spacing between inputs
        , div
            [ style "width" "10px"
            , style "height" "20px"

            -- , style "border-left" "thin solid black"
            ]
            []

        -- spacing between inputs
        , sampleTextField sampleTextInput sampleTextMsg
        , sizeInput fontSize fontSizeMsg
        , div [ style "width" "10px" ] [] -- spacing
        , resetButton resetMsg
        ]
    ]


narrow searchInput searchInputMsg searchMsg resetMsg =
    [ div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        , style "margin" "1.5em"
        , style "padding" "5px 10px"
        , style "border" "thin solid black"
        , style "border-radius" "48px"
        ]
        [ searchField searchInput searchInputMsg searchMsg
        , div [ style "width" "10px" ] [] -- spacing
        , resetButton resetMsg
        ]
    ]


sampleTextField input msg =
    Html.input
        [ type_ "text"
        , placeholder "Sample text"
        , onInput msg
        , value input
        , style "border" "none"
        , style "flex-grow" "1"
        ]
        []


searchField input searchInputMsg searchMsg =
    Html.form
        -- used so that pressing Enter will submit the search
        [ onSubmit searchMsg
        , style "margin-block-end" "0"
        , style "display" "flex"
        , style "justify-content" "space-between"
        , style "flex-grow" "1" -- TODO remove?
        ]
        [ Html.input
            [ type_ "text" -- using text, not search, so that "border: none" has an effect
            , onInput searchInputMsg
            , value input
            , placeholder "Search fonts"
            , style "border" "none"
            , style "flex-grow" "1"
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]


sizeInput fontSize fontSizeMsg =
    label []
        [ select [ onInput fontSizeMsg ]
            (List.map
                (\size ->
                    option
                        [ Html.Attributes.value size
                        , selected (size == fontSize)
                        ]
                        [ text size ]
                )
                [ "20px", "24px", "32px", "40px" ]
             -- sizes
            )
        ]


resetButton resetMsg =
    button [ onClick resetMsg ] [ text "Reset" ]
