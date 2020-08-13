import Html exposing (..)
import Html.Attributes exposing (style, rel, href, type_, placeholder)
import Html.Events exposing (onClick, onInput)
import Browser
import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode exposing (..)
import Http exposing (expectJson)
import List.Extra exposing (groupsOf)



-- MODEL

type alias Fonts = List Font

type alias Font =
  { family : String
  , category : String
  }

type alias Model =
  { fonts : WebData Fonts
  , visibleFonts : Fonts
  , restOfFonts : Fonts
  , text : String
  } -- perhaps refactor so that visible fonts is part of fonts

n = 8 -- number of fonts to get at a time
defaultText = "Making the Web Beautiful!"

init : () -> ( Model, Cmd Msg )
init _ =
  ( { fonts = Loading
    , visibleFonts = []
    , restOfFonts = []
    , text = defaultText
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
      , br [] []
      , br [] []
      , fontsView model.visibleFonts model.text
      , button [ onClick MoreFonts ] [ text "More" ]
      ]
 }

fontsView fonts text_ =
  div [] (List.map (fontView text_) fonts)

fontView text_ { family, category } =
  div [] [
    div [] [text family]
    , div [style "font-family" ("'" ++ family ++ "', " ++ category)] [text text_]
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



-- DECODERS

decodeFonts : Decoder Fonts
decodeFonts =
    at ["items"] (list decodeFont)

decodeFont : Decoder Font
decodeFont =
  map2 Font
    (field "family" string)
    (field "category" string)
  -- an object with "family" and "category" fields



-- MAIN

main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
