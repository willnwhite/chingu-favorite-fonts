import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Browser
import RemoteData exposing (WebData, RemoteData(..))
import Http exposing (expectJson)
import List.Extra exposing (groupsOf)

import Fonts exposing (..)


-- MODEL

type alias Model =
  { fonts : WebData Fonts
  , visibleFonts : Fonts
  , restOfFonts : Fonts
  , text : String
  , fontSize : String
  } -- perhaps refactor so that everything dependent on the list of fonts being fetched successfully is part of fonts

n = 8 -- number of fonts to get at a time
defaultText = "Making the Web Beautiful!"

init : () -> ( Model, Cmd Msg )
init _ =
  ( { fonts = Loading
    , visibleFonts = []
    , restOfFonts = []
    , text = defaultText
    , fontSize = "40px"
    }
  , Http.get
      { url = "https://www.googleapis.com/webfonts/v1/webfonts?sort=popularity&key=AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4"
      , expect = expectJson (RemoteData.fromResult >> FontsResponse) decodeFonts
      } -- fetch fonts from Google
  )



-- UPDATE

type Msg =
  FontsResponse (WebData Fonts)
  | MoreFonts
  | TextInput String
  | FontSize String

update msg model =
  case msg of
    FontsResponse response ->
      case response of
        Success fonts ->
          ( { model | fonts = response
            , restOfFonts = List.drop n fonts -- members of the list after n
            , visibleFonts = List.take n fonts -- first n members of the list
            }
          , Cmd.none
          )

        _ ->
          ( { model | fonts = response }, Cmd.none)

    MoreFonts ->
      ( { model | visibleFonts = model.visibleFonts ++ List.take n model.restOfFonts
          , restOfFonts = List.drop n model.restOfFonts
        }
      , Cmd.none
      )

    TextInput text ->
      ( { model | text = if text == "" then defaultText else text }
      , Cmd.none
      )

    FontSize size ->
      ( { model | fontSize = size }
      , Cmd.none
      )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Favorite Fonts"
  , body = case model.fonts of
    NotAsked ->
      [text "Initialising"]
    Loading ->
      [text "Loading"]
    RemoteData.Failure err ->
      [text ("Error: " ++ Debug.toString err)]
    Success allFonts ->
      [ div [] (List.map link (groupsOf n model.visibleFonts))
      , label [] [text "Text ", input [type_ "text", placeholder "Making the Web Beautiful!", onInput TextInput] []]
      , label []
          [ text " Font size "
          , select [onInput FontSize]
              [ option [ Html.Attributes.value "20px" ] [text "20px"]
              , option [ Html.Attributes.value "24px"] [text "24px"]
              , option [ Html.Attributes.value "32px"] [text "32px"]
              , option [ Html.Attributes.value "40px", selected True ] [text "40px"]
              ]
          ]
      , br [] []
      , br [] []
      , fontsView model.visibleFonts model.text model.fontSize
      , button [ onClick MoreFonts ] [ text "More" ]
      ]
 }

fontsView fonts text_ fontSize =
  div [] (List.map (fontView text_ fontSize) fonts)

fontView text_ fontSize { family, category } =
  div [] [
    div [] [text family]
    , div
        [ style "font-family" ("'" ++ family ++ "', " ++ category)
        , style "font-size" fontSize
        ]
        [text text_]
    , br [] []
  ]

link : Fonts -> Html msg
link fonts =
  let
      fontsJoined = List.foldl (\a b -> a ++ "|" ++ b) "" (List.map .family fonts) -- font families joined with | as per API -- at the moment, | is also added to the end. List.intersperse may be able to help.
  in
      Html.node "link"
        [ rel "stylesheet"
        , href ("https://fonts.googleapis.com/css?family=" ++ fontsJoined) -- TODO put +s between words and URL encode
        ] []



-- MAIN

main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
