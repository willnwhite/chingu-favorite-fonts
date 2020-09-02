module Fonts exposing (Fonts, append, decoder, drop, except, map, none, search, take, view)

import Font exposing (Font)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)
import ListExcept


type Fonts
    = Fonts (List Font)


take : Int -> Fonts -> Fonts
take n (Fonts fonts) =
    Fonts (List.take n fonts)


drop : Int -> Fonts -> Fonts
drop n (Fonts fonts) =
    Fonts (List.drop n fonts)


append : Fonts -> Fonts -> Fonts
append (Fonts fonts) (Fonts fonts2) =
    Fonts (List.append fonts fonts2)


except : Fonts -> Fonts -> Fonts
except (Fonts fonts) (Fonts fonts2) =
    Fonts (ListExcept.except fonts fonts2)


map : (Font -> b) -> Fonts -> List b
map function (Fonts fonts) =
    -- if map not used for anything except families, and Font.family not used anywhere else, then forget map and re-instate families
    List.map function fonts



-- families : Fonts -> List Font.FontFamily
-- families (Fonts fonts) =
--     List.map Font.family fonts


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
    Json.Decode.map Fonts (at [ "items" ] (list Font.decoder))


view : Fonts -> String -> String -> Html msg
view (Fonts fonts) sampleText fontSize =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
        ]
        (List.map (Font.view sampleText fontSize) fonts)
