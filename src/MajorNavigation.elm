module MajorNavigation exposing (Model, Msg(..), fontSize, init, sampleTextInput, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- CONFIGURATION


defaultFontSize =
    "32px"



-- MODEL


type Model
    = Model
        { searchInput : String -- what's typed into the Search field
        , sampleTextInput : String -- what's typed into the Sample text field
        , fontSize : String -- the selected font size
        }


sampleTextInput : Model -> String
sampleTextInput (Model model) =
    model.sampleTextInput


fontSize : Model -> String
fontSize (Model model) =
    model.fontSize


type Msg
    = SearchInput String
    | Search String
    | SampleTextInput String
    | FontSize String
    | Reset


init : Model
init =
    Model
        { searchInput = ""
        , sampleTextInput = ""
        , fontSize = defaultFontSize
        }


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        SearchInput input ->
            Model { model | searchInput = input }

        -- Search is picked up by Main through (MjrNav : Main.Msg)
        Search _ ->
            Model model

        SampleTextInput input ->
            Model { model | sampleTextInput = input }

        FontSize size ->
            Model { model | fontSize = size }

        Reset ->
            init


view : Model -> Int -> Html Msg
view (Model model) windowWidth =
    if windowWidth >= (300 * 2 + 31) then
        -- hand-tuned to match Fonts.view
        wide model.searchInput model.sampleTextInput model.fontSize

    else
        narrow model.searchInput


wide : String -> String -> String -> Html Msg
wide searchInput sampleTextInput_ fontSize_ =
    div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        , style "margin" "1.5em"
        , style "padding" "5px 10px"
        , style "border" "thin solid black"
        , style "border-radius" "48px"
        ]
        [ searchField searchInput
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
        , sampleTextField sampleTextInput_
        , sizeInput fontSize_
        , div [ style "width" "10px" ] [] -- spacing
        , resetButton
        ]


narrow : String -> Html Msg
narrow searchInput =
    div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        , style "margin" "1.5em"
        , style "padding" "5px 10px"
        , style "border" "thin solid black"
        , style "border-radius" "48px"
        ]
        [ searchField searchInput
        , div [ style "width" "10px" ] [] -- spacing
        , resetButton
        ]


sampleTextField : String -> Html Msg
sampleTextField input =
    Html.input
        [ type_ "text"
        , placeholder "Sample text"
        , onInput SampleTextInput
        , value input
        , style "border" "none"
        , style "flex-grow" "1"
        ]
        []


searchField : String -> Html Msg
searchField input =
    Html.form
        -- used so that pressing Enter will submit the search
        [ onSubmit (Search input)
        , style "margin-block-end" "0"
        , style "display" "flex"
        , style "justify-content" "space-between"
        , style "flex-grow" "1" -- TODO remove?
        ]
        [ Html.input
            [ type_ "text" -- using text, not search, so that "border: none" has an effect
            , onInput SearchInput
            , value input
            , placeholder "Search fonts"
            , style "border" "none"
            , style "flex-grow" "1"
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]


sizeInput : String -> Html Msg
sizeInput fontSize_ =
    label []
        [ select [ onInput FontSize ]
            (List.map
                (\size ->
                    option
                        [ Html.Attributes.value size
                        , selected (size == fontSize_)
                        ]
                        [ text size ]
                )
                [ "20px", "24px", "32px", "40px" ]
             -- sizes
            )
        ]


resetButton : Html Msg
resetButton =
    button [ onClick Reset ] [ text "Reset" ]
