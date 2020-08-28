module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


wideOrNarrow windowWidth =
    if windowWidth >= 720 then
        -- any less and "Catalog" comes too close to "Favorite Fonts"
        wide

    else
        narrow


wide =
    Html.header
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "margin-left" "1.5em"
        , style "margin-right" "1.5em"
        , style "border-bottom" "thin solid gray"
        ]
        [ h1 [] [ text "Favorite Fonts" ]
        , nav
            [ style "display" "flex"
            , style "justify-content" "space-around"
            , style "align-items" "center"
            ]
            [ a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "Catalog" ]
            , a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "Featured " ]
            , a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "Articles " ]
            , a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "About" ]
            ]
        ]


narrow =
    Html.header
        [ style "margin-left" "1.5em"
        , style "margin-right" "1.5em"
        , style "border-bottom" "thin solid gray"
        , style "padding-bottom" "1.5em"
        ]
        [ h1
            [ style "margin-top" "29px" -- hand-tuned to match text in wide header
            ]
            [ text "Favorite Fonts" ]
        , nav
            []
            [ a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "Catalog" ]
            , a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "Featured " ]
            , a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "Articles " ]
            , a [ href "", style "margin" "0 1.5em 0", style "text-decoration" "none" ] [ text "About" ]
            ]
        ]
