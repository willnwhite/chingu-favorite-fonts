module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Browser
import Browser.Dom
import Task
import RemoteData exposing (WebData, RemoteData(..))
import Http exposing (expectJson)
import List.Extra exposing (groupsOf)
import Url.Builder exposing (string) -- encodes URLs

import Fonts exposing (Font, Fonts)



-- MODEL

type alias Model =
  { allFonts : WebData Fonts
  , fontsForLinks : List (List String)
  , visibleFonts : Fonts
  , restOfFonts : Fonts
  , sampleText : String
  , fontSize : String
  , searchInput : String
  , searchResults : Fonts
  , showAllOrResults : View
  } -- perhaps refactor so that everything dependent on the list of fonts being fetched successfully is part of fonts -- same for searchResults: it should only exist if there's a search (not "")

type View = All | SearchResults

n = 8 -- number of fonts to get at a time
defaultText = "Making the Web Beautiful!"
defaultFontSize = "32px"

init : () -> ( Model, Cmd Msg )
init _ =
  ( { allFonts = Loading
    , fontsForLinks = [] -- fonts for each link element's href
    , visibleFonts = []
    , restOfFonts = []
    , sampleText = ""
    , fontSize = defaultFontSize
    , searchInput = ""
    , searchResults = []
    , showAllOrResults = All
    }
  , Http.get
      -- get a list of the font families currently available
      { url =
          -- https://www.googleapis.com/webfonts/v1/webfonts?sort=popularity&key=AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4
          Url.Builder.crossOrigin "https://www.googleapis.com" ["webfonts", "v1", "webfonts"] [string "sort" "popularity", string "key" "AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4"]
      , expect =
          expectJson (RemoteData.fromResult >> FontsResponse) Fonts.decodeFonts
      }
  )



-- UPDATE

type Msg =
  FontsResponse (WebData Fonts)
  | MoreFonts
  | SampleText String
  | FontSize String
  | SearchInput String
  | Search
  | Reset
  | BackToTop
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
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
      ( { model | sampleText = text }
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
      -- each time there's a new search, there'll be a new link (with a new href (new set of fonts to request)). so it'll look like [["Font 1", "Font 2"], ["Font 3", "Font 4"]], with no duplication of fonts. each sublist will be for one link.
      -- the search will determine which fonts are needed. then we'll look at the fonts that have already been requested (perhaps by flattening the [[]] data structure for the existing links), take out any that have been requested, and stick the new-to-request fonts on the end of that [[]] structure.
      case model.allFonts of
        Success allFonts ->
          let
            searchResults = List.filter (.family >> String.contains model.searchInput) allFonts -- look for all the fonts in allFonts that match (consider case sensitivity, punctuation, etc (maybe use a fuzzy library))
          in
            ( { model | searchResults = searchResults
              , fontsForLinks =
                  case fontsToRequest (List.concat model.fontsForLinks) (List.map .family searchResults) of
                    Just fontsForLink ->
                      model.fontsForLinks ++ [fontsForLink] -- there will be duplication here but it might not be a problem (slow things down)
                    Nothing ->
                      model.fontsForLinks
              , showAllOrResults = SearchResults
              }
            , Cmd.none
            )

        _ ->
          (model, Cmd.none)

    Reset ->
        ( { model | showAllOrResults = All
          , fontSize = defaultFontSize
          , sampleText = ""
          , searchInput = ""
          }
        , Cmd.none
        )

    BackToTop ->
      let
        resetViewport : Cmd Msg
        resetViewport =
            Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 0)
      in
        ( model, resetViewport )

    NoOp ->
      (model, Cmd.none)

-- test: when there are no fonts to request over what's already been requested, just return the original fontsForLinks, not fontsForLinks with empty lists in it.
-- > import Main
-- > Main.fontsToRequest ["a","b"] ["a"]
-- Nothing : Maybe (List String)
-- > Main.fontsToRequest ["a","b"] ["c"]
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
  , body =
      [ case model.allFonts of
          NotAsked ->
            text "Initialising"
          Loading ->
            text "Loading"
          RemoteData.Failure err ->
            text ("Error: " ++ Debug.toString err)
          Success allFonts ->
            main_ []
              ( [ nav []
                  [ a [href "", style "margin" "0 1.5em 0"] [text "Catalog"]
                  , a [href "", style "margin" "0 1.5em 0"] [text "Featured "]
                  , a [href "", style "margin" "0 1.5em 0"] [text "Articles "]
                  , a [href "", style "margin" "0 1.5em 0"] [text "About"]
                  ]
                , br [] []
                , div [] (List.map stylesheetLink ((groupsOf n << List.map .family) model.visibleFonts))
                , label [] [text "Text ", input [type_ "text", placeholder defaultText, onInput SampleText, value model.sampleText ] []]
                , label []
                    [ text " Font size "
                    , select [onInput FontSize]
                        (List.map (\size ->
                          option
                            [ Html.Attributes.value size
                            , selected (size == model.fontSize)
                            ]
                            [ text size ]
                          )
                          [ "20px", "24px", "32px", "40px" ] -- sizes
                        )
                    ]
                , Html.form [ onSubmit Search ]
                    [ label []
                      [ text " Font search "
                      , input [ type_ "search", onInput SearchInput, value model.searchInput ] []
                      , button [type_ "submit"] [text "Search"]
                      ]
                    ]
                , button [ onClick Reset ] [text "Reset"]
                , br [] []
                , br [] []
                ] ++
                  (case model.showAllOrResults of
                    All ->
                      [ fontsView model.visibleFonts (if model.sampleText == "" then defaultText else model.sampleText) model.fontSize
                      , button [ onClick MoreFonts ] [ text "More" ]
                      ]
                    SearchResults ->
                      [ div [] (List.map stylesheetLink model.fontsForLinks) -- the fact that this isn't shared between both All and SearchResults could mean that it's being requested again each time SearchResults is toggled to.
                      , fontsView model.searchResults (if model.sampleText == "" then defaultText else model.sampleText) model.fontSize
                      ]
                )
              ++
                [ button
                  [ style "position" "fixed"
                  , style "bottom" "0"
                  , style "right" "0"
                  , onClick BackToTop
                  ]
                  [text "Back to top"]
                ]
              )
      , footer [] [text "Made by Will White"]
      ]
 }

fontsView fonts text_ fontSize =
  div [] (List.map (fontView text_ fontSize) fonts)

fontView text_ fontSize { family, category } =
  div []
    [ div [] [text family]
    , div
        [ style "font-family" ("'" ++ family ++ "', " ++ category)
        , style "font-size" fontSize
        ]
        [text text_]
    , button [] [text "Add"]
    , br [] []
    , br [] []
  ]

stylesheetLink : List String -> Html msg
stylesheetLink fontFamilies =
  let
      fontRequestUrl =
        Url.Builder.crossOrigin
          -- https://fonts.googleapis.com/css?family=..."
          "https://fonts.googleapis.com" ["css"] [Url.Builder.string "family" (familyUrlParameter fontFamilies)]
  in
      -- <link rel="stylesheet" href="...">
      Html.node "link" [ rel "stylesheet", href fontRequestUrl ] []

familyUrlParameter : List String -> String
familyUrlParameter =
  -- ["Open Sans","Roboto"] -> "Open+Sans|Roboto"
  List.map (String.replace " " "+") >> String.join "|"



-- MAIN

main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
