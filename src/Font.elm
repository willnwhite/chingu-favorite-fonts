module Font exposing (Font, decodeFonts, family, view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)


type alias Font =
    { family : String
    , category : String
    }


family =
    .family



-- DECODERS
-- JSON structure to decode:
-- {"items": [{ "family": "Roboto", "category": "sans-serif", ... }], ...}


decodeFonts : Decoder (List Font)
decodeFonts =
    at [ "items" ] (list decodeFont)


decodeFont : Decoder Font
decodeFont =
    map2 Font
        (field "family" string)
        (field "category" string)



-- HTML


view : String -> String -> Font -> Html msg
view text_ fontSize font =
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
            [ style "font-family" ("'" ++ font.family ++ "', " ++ font.category)
            , style "font-size" fontSize
            ]
            [ text text_ ]
        ]
