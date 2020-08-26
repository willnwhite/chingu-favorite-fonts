port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Browser
import Browser.Dom
import Browser.Events
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
  , searchString : String
  , searchResults : Fonts
  , showAllOrResults : View
  , windowWidth : Int
  , scrollPosition : Float
  } -- perhaps refactor so that everything dependent on the list of fonts being fetched successfully is part of fonts -- same for searchResults: it should only exist if there's a search (not "")

type View = All | SearchResults

n = 8 -- number of fonts to get at a time
defaultText = "Making the Web Beautiful!"
defaultFontSize = "32px"

init : Int -> ( Model, Cmd Msg )
init windowWidth =
  ( { allFonts = Loading
    , fontsForLinks = [] -- fonts for each link element's href
    , visibleFonts = []
    , restOfFonts = []
    , sampleText = ""
    , fontSize = defaultFontSize
    , searchString = ""
    , searchResults = []
    , showAllOrResults = All
    , windowWidth = windowWidth
    , scrollPosition = 0
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



-- PORTS

type alias Viewport =
  -- Elm Browser.Dom terminology / JS DOM terminology
  { sceneHeight : Float -- scrollHeight
  , viewportHeight : Float -- clientHeight
  , viewportY : Float -- scrollTop
  }

port scroll : (Viewport -> msg) -> Sub msg

port getViewport : () -> Cmd msg
port gotViewport : (Viewport -> msg) -> Sub msg
-- Browser.Dom.getViewport has an issue (https://github.com/elm/browser/issues/118). These ports are a stand-in.


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
  | WindowResize Int Int
  | Scroll Viewport
  | GotViewport Viewport

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
          -- , Cmd.none
          , getViewport ()
          -- model changed, view should be updated, now check whether we're at the bottom of the page or not (sceneHeight == viewportHeight) (port to JS).
          )

        _ ->
          ( { model | allFonts = response }, Cmd.none)

    GotViewport {sceneHeight, viewportHeight} ->
      ( if Debug.log "scene" sceneHeight == Debug.log "viewport" viewportHeight then -- at bottom of page
          { model | visibleFonts = model.visibleFonts ++ List.take n model.restOfFonts
          , restOfFonts = List.drop n model.restOfFonts
          , fontsForLinks = model.fontsForLinks ++ [(List.take n >> List.map .family) model.restOfFonts]
          }
        else
          model
      , Cmd.none
      )

    MoreFonts ->
      ( { model | visibleFonts = model.visibleFonts ++ List.take n model.restOfFonts
        , restOfFonts = List.drop n model.restOfFonts
        , fontsForLinks = model.fontsForLinks ++ [(List.take n >> List.map .family) model.restOfFonts]
        }
      -- , Cmd.none
      -- model changed, view updated, now check whether you're at the bottom of the page or not. if so, request more fonts. this will be useful if the user has a very tall screen, and loading more fonts hasn't yet filled the screen.
      , getViewport ()
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
          ( { model | searchString = input
            , showAllOrResults = All
            }
          , Cmd.none
          )
        _ ->
          ( { model | searchString = input }, Cmd.none )

    Search ->
      -- each time there's a new search, there'll be a new link (with a new href (new set of fonts to request)). so it'll look like [["Font 1", "Font 2"], ["Font 3", "Font 4"]], with no duplication of fonts. each sublist will be for one link.
      -- the search will determine which fonts are needed. then we'll look at the fonts that have already been requested (perhaps by flattening the [[]] data structure for the existing links), take out any that have been requested, and stick the new-to-request fonts on the end of that [[]] structure.
      case model.allFonts of
        Success allFonts ->
          let
            searchResults = List.filter (.family >> String.contains model.searchString) allFonts -- look for all the fonts in allFonts that match (consider case sensitivity, punctuation, etc (maybe use a fuzzy library))
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
          , searchString = ""
          }
        , Cmd.none
        )

    Scroll { sceneHeight, viewportHeight, viewportY } ->
      ( case model.showAllOrResults of
          All ->
            if viewportY + viewportHeight >= sceneHeight then -- at bottom of page
              { model | visibleFonts = model.visibleFonts ++ List.take n model.restOfFonts
              , restOfFonts = List.drop n model.restOfFonts
              , fontsForLinks = model.fontsForLinks ++ [(List.take n >> List.map .family) model.restOfFonts]
              , scrollPosition = viewportY
              }
            else
              { model | scrollPosition = viewportY }

          SearchResults ->
            model

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

    WindowResize width _ ->
      ( { model | windowWidth = width }, Cmd.none)

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



-- SUBSCRIPTIONS

subscriptions model =
  Sub.batch
    [ scroll Scroll
    , Browser.Events.onResize WindowResize
    , gotViewport GotViewport
    ]



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Favorite Fonts"
  , body =
      [ case model.allFonts of
          NotAsked ->
            text "Initialising..."
          Loading ->
            text "Fetching up-to-date fonts..."
          RemoteData.Failure err ->
            text ("Error: " ++ Debug.toString err)
          Success allFonts ->
            div [ id "scroll", style "font-family" "sans-serif", style "overflow" "hidden" ]
              [ header model.windowWidth
              , div [] (List.map stylesheetLink ((groupsOf n << List.map .family) model.visibleFonts))
              , main_ [ style "margin-bottom" "1.5em"]
                  (
                    majorNavigation model.windowWidth model.searchString model.sampleText model.fontSize
                    ++
                    (case model.showAllOrResults of
                      All ->
                        [ fontsView model.visibleFonts (if model.sampleText == "" then defaultText else model.sampleText) model.fontSize
                        ]
                      SearchResults ->
                        [ div [] (List.map stylesheetLink model.fontsForLinks) -- the fact that this isn't shared between both All and SearchResults could mean that it's being requested again each time SearchResults is toggled to.
                        , fontsView model.searchResults (if model.sampleText == "" then defaultText else model.sampleText) model.fontSize
                        ]
                    )
                    ++
                    (if model.scrollPosition >= 300 then
                      [ button
                        [ style "position" "fixed"
                        , style "bottom" "5em"
                        , style "right" "1.5em"
                        , onClick BackToTop
                        ]
                        [text "Back to top"]
                      ]
                    else
                      []
                    )
                  )
              , footer
                  [ style "position" "fixed"
                  , style "bottom" "0"
                  , style "right" "0"
                  , style "left" "0"
                  , style "height" "2em"
                  , style "background" "white"
                  , style "padding-top" "1em"
                  ]
                  [ div [style "text-align" "center"] [text "Made by Will White"]]
            ]
      ]
 }

header windowWidth =
  if windowWidth >= 720 then -- any less and "Catalog" comes too close to "Favorite Fonts"
    wideHeader
  else
    narrowHeader

wideHeader =
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
      [ a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "Catalog"]
      , a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "Featured "]
      , a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "Articles "]
      , a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "About"]
      ]
    ]

narrowHeader =
  Html.header
    [ style "margin-left" "1.5em"
    , style "margin-right" "1.5em"
    , style "border-bottom" "thin solid gray"
    , style "padding-bottom" "1.5em" ]
    [ h1
        [ style "margin-top" "29px" -- hand-tuned to match text in wide header
        ] [ text "Favorite Fonts" ]
    , nav
        []
        [ a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "Catalog"]
        , a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "Featured "]
        , a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "Articles "]
        , a [href "", style "margin" "0 1.5em 0", style "text-decoration" "none"] [text "About"]
        ]
    ]

majorNavigation windowWidth searchString sampleText fontSize =
  if windowWidth >= (300 * 2 + 31) then -- hand-tuned to match fontsView
    wideMajorNavigation searchString sampleText fontSize
  else
    narrowMajorNavigation searchString

wideMajorNavigation searchString sampleText fontSize =
  [ div
      [ style "display" "flex"
      , style "justify-content" "space-between"
      , style "align-items" "center"
      , style "margin" "1.5em"
      , style "padding" "5px 10px"
      , style "border" "thin solid black"
      , style "border-radius" "48px"
      ]
      [ searchInput searchString
      , div [ style "width" "10px"
            , style "height" "20px"
            , style "border-right" "thin solid black"
            ] [] -- spacing between inputs
      , div [ style "width" "10px"
            , style "height" "20px"
            -- , style "border-left" "thin solid black"
            ] [] -- spacing between inputs
      , sampleTextInput sampleText
      , sizeInput fontSize
      , div [ style "width" "10px" ] [] -- spacing
      , resetButton
      ]
  ]

narrowMajorNavigation searchString =
  [ div
      [ style "display" "flex"
      , style "justify-content" "space-between"
      , style "align-items" "center"
      , style "margin" "1.5em"
      , style "padding" "5px 10px"
      , style "border" "thin solid black"
      , style "border-radius" "48px"
      ]
      [ searchInput searchString
      , div [ style "width" "10px" ] [] -- spacing
      , resetButton
      ]
  ]

sampleTextInput sampleText =
  input
    [ type_ "text"
    , placeholder "Sample text"
    , onInput SampleText
    , value sampleText
    , style "border" "none"
    , style "flex-grow" "1"
    ] []

searchInput searchString =
  Html.form [ onSubmit Search, style "margin-block-end" "0", style "flex-grow" "1", style "display" "flex", style "justify-content" "space-between" ]
    [ input
      [ type_ "text" -- using text, not search, so that "border: none" has an effect
      , onInput SearchInput
      , value searchString
      , placeholder "Search fonts"
      , style "border" "none"
      , style "flex-grow" "1"
      ] []
    , button [type_ "submit"] [text "Search"]
    ]

sizeInput fontSize =
  label []
      [ select [onInput FontSize]
          (List.map (\size ->
            option
              [ Html.Attributes.value size
              , selected (size == fontSize)
              ]
              [ text size ]
            )
            [ "20px", "24px", "32px", "40px" ] -- sizes
          )
      ]

resetButton =
  button [ onClick Reset ] [text "Reset"]

fontsView fonts text_ fontSize =
  div
    [ style "display" "grid"
    , style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
    ]
    (List.map (fontView text_ fontSize) fonts)

fontView text_ fontSize { family, category } =
  div
    [ style "border-top" "thin solid black"
    , style "margin" "1.5em"
    , style "padding-top" "0.5em"
    ]
    [ div
      [ style "display" "flex", style "justify-content" "space-between"
      , style "align-items" "flex-start" -- stops Add button stretching heightwise when font family text is over more than one line
      ]
      [ div [] [text family]
      , button [] [text "Add"]
      ]
    , div
        [ style "font-family" ("'" ++ family ++ "', " ++ category)
        , style "font-size" fontSize
        ]
        [text text_]
  ]

stylesheetLink : List String -> Html msg
stylesheetLink fontFamilies =
  -- <link rel="stylesheet" href="...">
  Html.node "link" [ rel "stylesheet", href (fontRequestUrl fontFamilies) ] []

fontRequestUrl fontFamilies =
  "https://fonts.googleapis.com/css?family=" ++ familyUrlParameter fontFamilies

familyUrlParameter : List String -> String
familyUrlParameter =
  -- ["Open Sans","Roboto"] -> "Open+Sans|Roboto"
  List.map (String.replace " " "+") >> String.join "|"



-- MAIN

main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
