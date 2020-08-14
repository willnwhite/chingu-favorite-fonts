module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Browser
import RemoteData exposing (WebData, RemoteData(..))
import Http exposing (expectJson)
import List.Extra exposing (groupsOf)

import Fonts exposing (..)



-- MODEL

type alias Model =
  { allFonts : WebData Fonts
  , fontsForLinks : List (List String)
  , visibleFonts : Fonts
  , restOfFonts : Fonts
  , text : String
  , fontSize : String
  , searchInput : String
  , searchResults : Fonts
  , showAllOrResults : View
  } -- perhaps refactor so that everything dependent on the list of fonts being fetched successfully is part of fonts -- same for searchResults: it should only exist if there's a search (not "")

type View = All | Results

n = 8 -- number of fonts to get at a time
defaultText = "Making the Web Beautiful!"

init : () -> ( Model, Cmd Msg )
init _ =
  ( { allFonts = Loading
    , fontsForLinks = [] -- fonts for each link element's href
    , visibleFonts = []
    , restOfFonts = []
    , text = defaultText
    , fontSize = "40px"
    , searchInput = ""
    , searchResults = []
    , showAllOrResults = All
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
  | SampleText String
  | FontSize String
  | SearchInput String
  | Search

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    FontsResponse response ->
      case response of
        Success fonts ->
          let
              fontsForLink = (List.take n >> List.map .family) fonts -- first n members of the list
          in
          ( { model | allFonts = response
            , restOfFonts = List.drop n fonts -- members of the list after n
            , visibleFonts = List.take n fonts -- first n members of the list
            , fontsForLinks = model.fontsForLinks ++ [fontsForLink]
            }
          , Cmd.none
          )

        _ ->
          ( { model | allFonts = response }, Cmd.none)

    MoreFonts ->
      ( { model | visibleFonts = model.visibleFonts ++ List.take n model.restOfFonts
        , restOfFonts = List.drop n model.restOfFonts
        , fontsForLinks = model.fontsForLinks ++ [(List.take n >> List.map .family) model.restOfFonts]
        }
      , Cmd.none
      )

    SampleText text ->
      ( { model | text = if text == "" then defaultText else text }
      , Cmd.none
      )

    FontSize size ->
      ( { model | fontSize = size }
      , Cmd.none
      )

    SearchInput input ->
      case input of
        "" ->
          ( { model | searchInput = input
            , showAllOrResults = All
            }
          , Cmd.none
          )
        _ ->
          ( { model | searchInput = input }, Cmd.none )

    Search ->
      -- each time there's a new letter, there'll be a new link (with a new href (new set of fonts to request)). so it'll look like [["Font 1", "Font 2"], ["Font 3", "Font 4"]], with no duplication of fonts. each sublist will be for one link. the search will determine which fonts are needed. then we'll look at the fonts that have already been requested (perhaps by flattening the [[]] data structure for the existing links), take out any that have been, and stick the new-to-request fonts on the end of that [[]] structure. that [[]] needs to be maintained, else the links would change and you'd end up requesting things again.
      case model.allFonts of
        Success allFonts ->
          let
              searchResults = List.filter (.family >> String.contains model.searchInput) allFonts -- get the search string, look (filter?) for all the fonts in allFonts that match (contain the substring) (consider case sensitivity, punctuation, etc (maybe use a fuzzy library))
          in
          ( { model | searchResults = searchResults
            , fontsForLinks =
                case fontsToRequest (List.concat model.fontsForLinks) (List.map .family searchResults) of
                  Just fontsForLink ->
                    model.fontsForLinks ++ [fontsForLink] -- there will be duplication here but it might not be a problem (slow things down)
                  Nothing ->
                    model.fontsForLinks
            , showAllOrResults = Results
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

-- write a test: when there are no fonts to request over what's already been requested, just return the original fontsForLinks, not fontsForLinks with empty lists in it.
-- > import Main
-- > Main.fontsForLink_ ["a","b"] ["a"]
-- Nothing : Maybe (List String)
-- > Main.fontsForLink_ ["a","b"] ["c"]
-- Just ["c"] : Maybe (List String)

fontsToRequest : List String -> List String -> Maybe (List String) -- explain why this Maybe is necessary, all the way back to the links
fontsToRequest fontsAlreadyRequested fontsNeeded =
  let
      i = List.filter (\result -> not (List.member result fontsAlreadyRequested)) fontsNeeded
  in
      if List.isEmpty i then Nothing else Just i
  -- filter out fonts needed that have already been requested.



-- (write a test: when there are fonts to request over what's already been requested, only the fonts that haven't already been requested will be returned.)




-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Favorite Fonts"
  , body = case model.allFonts of
    NotAsked ->
      [text "Initialising"]
    Loading ->
      [text "Loading"]
    RemoteData.Failure err ->
      [text ("Error: " ++ Debug.toString err)]
    Success allFonts ->
          [ div [] (List.map link ((groupsOf n << List.map .family) model.visibleFonts))
          , label [] [text "Text ", input [type_ "text", placeholder "Making the Web Beautiful!", onInput SampleText] []]
          , label []
              [ text " Font size "
              , select [onInput FontSize]
                  [ option [ Html.Attributes.value "20px" ] [text "20px"]
                  , option [ Html.Attributes.value "24px"] [text "24px"]
                  , option [ Html.Attributes.value "32px"] [text "32px"]
                  , option [ Html.Attributes.value "40px", selected True ] [text "40px"]
                  ]
              ]
          , Html.form [ onSubmit Search ]
              [ label []
                [ text " Font search "
                , input [ type_ "search", onInput SearchInput ] []
                , button [type_ "submit"] [text "Search"]
                ]
              ]
          , br [] []
          , br [] []
          ] ++
            (case model.showAllOrResults of
              All ->
                [ fontsView model.visibleFonts model.text model.fontSize
                , button [ onClick MoreFonts ] [ text "More" ]
                ]
              Results ->
                [ div [] (List.map link model.fontsForLinks)
                , fontsView model.searchResults model.text model.fontSize
                ]
          )
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

link : List String -> Html msg
link fontFamilies =
  let
      fontsJoined = List.foldl (\a b -> a ++ "|" ++ b) "" fontFamilies -- font families joined with | as per API -- at the moment, | is also added to the end. List.intersperse and/or String.fromList and/or String.join may be able to help.
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
