module Fonts exposing (..)

import Font exposing (Font)
import Html exposing (Html, div)
import Html.Attributes exposing (..)


type alias Fonts =
    List Font


view : Fonts -> String -> String -> Html msg
view fonts sampleText fontSize =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
        ]
        (List.map (Font.view sampleText fontSize) fonts)
