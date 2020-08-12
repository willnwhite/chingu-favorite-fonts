import Html exposing (..)
import Html.Attributes exposing (style, rel, href)
import Browser
import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode exposing (..)
import Http exposing (expectJson)



-- MODEL

type alias Fonts = List Font
type alias Font =
  { family : String
  , category : String
  }
type alias Model =
  { fonts : WebData Fonts
  , text : String
  }

init : () -> ( Model, Cmd Msg )
init _ =
  ( { fonts = Loading, text = "Making the Web Beautiful!" }
  , Http.get
      { url = "https://www.googleapis.com/webfonts/v1/webfonts?sort=popularity&key=AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4"
      , expect = expectJson (RemoteData.fromResult >> FontsResponse) decodeFonts
      } -- fetch fonts from Google
  )



-- UPDATE

type Msg = FontsResponse (WebData Fonts)

update msg model =
  case msg of
    FontsResponse response ->
      ( { model | fonts = response }
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
      let
          fonts = {-List.take 8 -}allFonts -- take gives first n members of the list
          -- TODO make one request for all the fonts
      in
      [div [] (List.map (\font ->
        div [] [
          Html.node "link" [rel "stylesheet"
          , href ("https://fonts.googleapis.com/css?family=" ++ font.family) -- TODO put +s between words and URL encode
          ] []
          , div [] [text font.family]
          , div [style "font-family" ("'" ++ font.family ++ "', " ++ font.category)] [text model.text]
          , br [] []
        ]

        )
        fonts)]
 }



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
