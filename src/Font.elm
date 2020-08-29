module Font exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)


type alias Font =
    { family : FontFamily
    , category : String
    }


type alias FontFamily =
    String


family =
    .family


category =
    .category



-- DECODER
-- JSON structure to decode:
-- { "family": "Roboto", "category": "sans-serif", ... }


decodeFont : Decoder Font
decodeFont =
    map2 Font
        (field "family" string)
        (field "category" string)



-- HTML


view : String -> String -> Font -> Html msg
view sampleText fontSize font =
    div
        [ style "border-top" "thin solid black"
        , style "margin" "1.5em"
        , style "padding-top" "0.5em"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "flex-start" -- stops Add button stretching heightwise when font family text is over more than one line
            ]
            [ div [] [ text font.family ]
            , button [] [ text "Add" ]
            ]
        , div
            [ style "font-family" ("'" ++ family font ++ "', " ++ category font)
            , style "font-size" fontSize
            ]
            [ text sampleText ]
        ]
