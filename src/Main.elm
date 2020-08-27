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

import Fonts exposing (Font)
import RequestedFonts exposing (RequestedFonts)



-- MODEL

type alias Model =
  { availableFonts : WebData (List Font) -- what fonts are available, from Google Fonts Developer API, sorted by popularity

  -- Requesting all available fonts at once caused page load problem, isn't polite, and isn't necessary. On page load, request enough fonts that they fill the page, and more will be loaded when the user scrolls to the bottom.

  -- if you get a font in a search result, that font shouldn't be "thrown in" to the sorted-by-popularity list, even if it's in the right place, because there's no guarantee there won't be fonts missing. Eg. sorted by popularity could show A, B, C, and search could return E. Now if you put E on the end of A, B, C, it looks like E is the next-most popular to C, when in fact that's D. Also, when D is loaded, E will jump down. you do, however, want to keep the links for any fonts the search has requested. that way, if any of the search results do pop up in the popularity list, they'll already have their stylesheets.

  , requestedFonts : RequestedFonts -- The same font should not be requested more than once (via link href or however). This could happen if a font is in a search result but it's already in the sorted-by-popularity list (or vice versa). Storing which fonts have already been requested means we can avoid requesting the same one again.
  -- RequestedFonts also records which fonts were requested together (multiple fonts can be requested per HTTP request). Each list of fonts goes to make up the HTTP request to request that list. If the HTTP request changes, then the DOM changes (because we're using link hrefs to request fonts), and if the DOM changes, the browser might re-request unnecessarily. Sure, a link with the same href might be served by the browser's cache, but that shouldn't be relied upon. Also, while working with link href, we'll have to assume that all requests are successful.
  {-
    <link href="...FontA|FontB|FontC">
    <link href="...FontD|FontE">
  -}

  -- TODO visibleFonts + restOfFonts is a duplication of availableFonts. more memory efficient to store an Int representing how far down the availableFonts list we are. This Int could either be part of availableFonts' type, or stored separately to reflect the fact that it's an optimisation. Or, this fact could just be documented in availableFonts' type, which would be even more memory efficient! Do that once you've got the former, simpler thing working. The trouble with the simpler thing is that you have to remember to update both availableFonts and the Int at the same time.
  , visibleFonts : Int

  -- and possibly, the font requests could be stored in availableFonts' type: (requested : Bool in the Font type), and groupings stored in availableFonts' type:
  -- availableFonts : { requested : List (List Font), unrequested : List Font }
  -- no, because then when searched-for fonts get in requested, how do you keep a record of which sorted-by-popularity fonts should be visible?

  , searchResults : List Font -- fonts that match the last search (subset of availableFonts)

  , searchString : String -- what's typed into the Search field
  , sampleText : String -- what's typed into the Sample text field
  , fontSize : String -- the selected font size
  , showAllOrResults : View -- necessary so that the view can choose between using searchResults and visibleFonts (all) as a data source
  , windowWidth : Int
  , scrollPosition : Float
  } -- perhaps refactor so that everything dependent on the list of fonts being fetched successfully is part of fonts -- same for searchResults: it should only exist if there's a search (not "")

type View = All | SearchResults

-- type VisibleFonts = VisibleFonts (List Font) Int

fontsPerRequest = 8 -- number of fonts to get at a time
defaultText = "Making the Web Beautiful!"
defaultFontSize = "32px"
apiKey = "AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4"

init : Int -> ( Model, Cmd Msg )
init windowWidth =
  ( { availableFonts = Loading
    , requestedFonts = []
    -- , visibleFonts = []
    -- , restOfFonts = []
    , visibleFonts = 0 -- or fontsPerRequest?
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
          -- https://www.googleapis.com/webfonts/v1/webfonts?sort=popularity&key=...
          Url.Builder.crossOrigin "https://www.googleapis.com" ["webfonts", "v1", "webfonts"] [string "sort" "popularity", string "key" apiKey]
      , expect =
          expectJson (RemoteData.fromResult >> FontsResponse) Fonts.decodeFonts
      }
  )



-- PORTS

type alias Viewport =
  { sceneHeight : Float
  , viewportHeight : Float
  , viewportY : Float
  }

-- Browser.Dom.getViewport has an issue (https://github.com/elm/browser/issues/118). These ports are a stand-in.

port getViewport : () -> Cmd msg

port viewport : (Viewport -> msg) -> Sub msg



-- UPDATE

type Msg =
  FontsResponse (WebData (List Font))
  -- | MoreFonts
  | SampleText String
  | FontSize String
  | SearchInput String
  | Search
  | Reset
  | BackToTop
  | NoOp
  | WindowResize Int Int
  | GotViewport Viewport

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.availableFonts of
    Success fonts ->
      case msg of
        -- MoreFonts ->
        --   ( { model |
        --     -- visibleFonts = model.visibleFonts ++ List.take fontsPerRequest model.restOfFonts
        --     visibleFonts = model.visibleFonts + fontsPerRequest
        --     -- , restOfFonts = List.drop fontsPerRequest model.restOfFonts
        --     , requestedFonts = RequestedFonts.update model.requestedFonts ((List.take fontsPerRequest >> List.map .family) model.restOfFonts)
        --     }
        --   -- model changed, view updated, now check whether you're at the bottom of the page or not. if so, request more fonts. this will be useful if the user has a very tall screen, and loading more fonts hasn't yet filled the screen.
        --   , getViewport ()
        --   )

        GotViewport { sceneHeight, viewportHeight, viewportY } ->
          -- if we're at the bottom of the page, request some more fonts
          ( case model.showAllOrResults of
              All ->
                if viewportY + viewportHeight >= sceneHeight then -- at bottom of page
                  let
                      restOfFonts = List.drop model.visibleFonts fonts -- members of the list after fontsPerRequest
                  in

                  { model |
                  -- visibleFonts = model.visibleFonts ++ List.take fontsPerRequest model.restOfFonts
                  visibleFonts = model.visibleFonts + fontsPerRequest
                  -- , restOfFonts = List.drop fontsPerRequest model.restOfFonts
                  , requestedFonts = RequestedFonts.update model.requestedFonts ((List.take fontsPerRequest >> List.map .family) restOfFonts)
                  , scrollPosition = viewportY
                  }
                else
                  { model | scrollPosition = viewportY }

              SearchResults ->
                model

          , Cmd.none
          )

        Search ->
          -- the search will determine which fonts are needed. then we'll look at the fonts that have already been requested (perhaps by flattening the [[]] data structure for the existing links), take out any that have been requested, and stick the new-to-request fonts on the end of that [[]] structure.
          case model.availableFonts of
            Success availableFonts ->
              let
                searchResults = List.filter (.family >> String.toLower >> String.contains (String.toLower model.searchString)) availableFonts
              in
                ( { model | searchResults = searchResults
                  , requestedFonts = RequestedFonts.update model.requestedFonts (List.map .family searchResults)
                  , showAllOrResults = SearchResults
                  }
                , Cmd.none
                )

            _ ->
              ( model, Cmd.none )

        SearchInput input ->
          -- TODO refactor (showAllOrResults = case input of...)
          case input of
            "" ->
              ( { model | searchString = input
                , showAllOrResults = All
                }
              , Cmd.none
              )
            _ ->
              ( { model | searchString = input }, Cmd.none )

        SampleText text ->
          ( { model | sampleText = text }, Cmd.none )

        FontSize size ->
          ( { model | fontSize = size }, Cmd.none )

        Reset ->
            ( { model | showAllOrResults = All
              , fontSize = defaultFontSize
              , sampleText = ""
              , searchString = ""
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
          ( model, Cmd.none )

        WindowResize width _ ->
          ( { model | windowWidth = width }, Cmd.none )

        _ ->
          ( model, Cmd.none )

    _ ->
      case msg of
        FontsResponse response ->
          case response of
            Success fonts ->
              let
                  fontsToRequest = (List.take fontsPerRequest >> List.map .family) fonts -- first fontsPerRequest fonts
              in
              ( { model |
                availableFonts = response
                -- , restOfFonts = List.drop fontsPerRequest fonts -- members of the list after fontsPerRequest
                -- , visibleFonts = List.take fontsPerRequest fonts -- first fontsPerRequest members of the list
                , visibleFonts = model.visibleFonts + fontsPerRequest
                , requestedFonts = RequestedFonts.update model.requestedFonts fontsToRequest
                }
              , getViewport ()
              -- check whether we're at the bottom of the page or not
              )

            _ ->
              ( { model | availableFonts = response }, Cmd.none )

        _ ->
          ( model, Cmd.none )



-- SUBSCRIPTIONS

subscriptions model =
  Sub.batch
    [ viewport GotViewport
    , Browser.Events.onResize WindowResize
    -- , gotViewport GotViewport
    ]



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Favorite Fonts"
  , body =
      [ case model.availableFonts of
          NotAsked ->
            text "Initialising..."
          Loading ->
            text "Fetching up-to-date fonts..."
          RemoteData.Failure err ->
            text ("Error: " ++ Debug.toString err)
          Success fonts ->
            div [ id "scroll", style "font-family" "sans-serif", style "overflow" "hidden" ]
              [ header model.windowWidth
              , div [] (List.map stylesheetLink model.requestedFonts)
              , main_ [ style "margin-bottom" "1.5em"]
                  (
                    majorNavigation model.windowWidth model.searchString model.sampleText model.fontSize
                    ++
                    (case model.showAllOrResults of
                      All ->
                        [ fontsView (visibleFonts fonts model.visibleFonts) (if model.sampleText == "" then defaultText else model.sampleText) model.fontSize
                        ]
                      SearchResults ->
                        [ div [] (List.map stylesheetLink model.requestedFonts) -- the fact that this isn't shared between both All and SearchResults could mean that it's being requested again each time SearchResults is toggled to.
                        , fontsView model.searchResults (if model.sampleText == "" then defaultText else model.sampleText) model.fontSize
                        ]
                    )
                    ++
                    (if model.scrollPosition >= 300 then
                      [ backToTopButton ]
                    else
                      []
                    )
                  )
              , footer
            ]
      ]
 }

visibleFonts fonts n =
  List.take n fonts



-- HTML

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

fontsView : List Font -> String -> String -> Html msg
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

backToTopButton =
  button
    [ style "position" "fixed"
    , style "bottom" "5em"
    , style "right" "1.5em"
    , onClick BackToTop
    ]
    [text "Back to top"]

footer =
  Html.footer
    [ style "position" "fixed"
    , style "bottom" "0"
    , style "right" "0"
    , style "left" "0"
    , style "height" "2em"
    , style "background" "white"
    , style "padding-top" "1em"
    ]
    [ div [style "text-align" "center"] [text "Made by Will White"]]



-- STYLESHEET LINK

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
