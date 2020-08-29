module Fonts exposing (..)

import Font exposing (Font, decodeFont)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)


type alias Fonts =
    List Font


first =
    List.take


rest =
    List.drop


families =
    List.map Font.family


search : String -> (Fonts -> Fonts)
search searchInput =
    List.filter (Font.family >> String.toLower >> String.contains (String.toLower searchInput))


none =
    []



-- DECODER
-- JSON structure to decode:
-- {"items": [Font], ...}


decodeFonts : Decoder Fonts
decodeFonts =
    at [ "items" ] (list decodeFont)


view : Fonts -> String -> String -> Html msg
view fonts sampleText fontSize =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
        ]
        (List.map (Font.view sampleText fontSize) fonts)
