module Font exposing (Font, FontFamily, category, decoder, family, view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)


type Font
    = Font Font_


type alias Font_ =
    { family : FontFamily
    , category : String
    }


type alias FontFamily =
    String


family (Font font) =
    font.family


category (Font font) =
    font.category



-- DECODER
-- JSON to decode:
-- { "family": "Roboto", "category": "sans-serif", ... }


decoder : Decoder Font
decoder =
    -- TODO try to decode without using the Font_ type alias
    Json.Decode.map Font
        (map2
            Font_
            (field "family" string)
            (field "category" string)
        )



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
            [ div [] [ text (family font) ]
            , button [] [ text "Add" ]
            ]
        , div
            [ style "font-family" ("'" ++ family font ++ "', " ++ category font)
            , style "font-size" fontSize
            ]
            [ text sampleText ]
        ]
