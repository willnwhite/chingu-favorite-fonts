module Fonts exposing (Fonts, decoder, drop, families, none, search, take, view)

import Font exposing (Font)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)


type Fonts
    = Fonts (List Font)


take : Int -> Fonts -> Fonts
take n (Fonts fonts) =
    Fonts (List.take n fonts)


drop : Int -> Fonts -> Fonts
drop n (Fonts fonts) =
    Fonts (List.drop n fonts)


families (Fonts fonts) =
    List.map Font.family fonts


search : String -> Fonts -> Fonts
search searchInput (Fonts fonts) =
    Fonts (List.filter (Font.family >> String.toLower >> String.contains (String.toLower searchInput)) fonts)


none : Fonts
none =
    Fonts []



-- DECODER
-- JSON structure to decode:
-- {"items": [Font], ...}


decoder : Decoder Fonts
decoder =
    map Fonts (at [ "items" ] (list Font.decoder))


view : Fonts -> String -> String -> Html msg
view (Fonts fonts) sampleText fontSize =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
        ]
        (List.map (Font.view sampleText fontSize) fonts)
