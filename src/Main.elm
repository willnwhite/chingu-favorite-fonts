port module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Fonts exposing (Fonts)
import Header
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (expectJson)
import LoadedAndUnloadedFonts as LUFonts exposing (LoadedAndUnloadedFonts(..))
import MajorNavigation as MjrNav
import RemoteData exposing (RemoteData(..), WebData)
import RequestedFonts exposing (RequestedFonts)
import Task
import Url.Builder exposing (string)



-- CONFIGURATION


apiKey =
    "AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4"


fontsPerRequest =
    8


defaultSampleText =
    "Making the Web Beautiful!"



-- defaultFontSize =
--     "32px"
-- MODEL


type alias Model =
    { main : WebData Model_
    , windowWidth : Int
    }


type alias Model_ =
    { availableFonts : LoadedAndUnloadedFonts -- what fonts are available, from Google Fonts Developer API, sorted by popularity

    -- Requesting all available fonts at once caused page load problem, isn't polite, and isn't necessary. On page load, request enough fonts that they fill the page, and more will be loaded when the user scrolls to the bottom.
    -- and possibly, the font requests could be stored in availableFonts' type: (requested : Bool in the Font type), and groupings stored in availableFonts' type:
    -- availableFonts : { requested : List (List Font), unrequested : List Font }
    -- no, because then when searched-for fonts get in requested, how do you keep a record of which sorted-by-popularity fonts should be visible?
    , requestedFonts : RequestedFonts -- The same font should not be requested more than once (via link href or however). This could happen if a font is in a search result but it's already in the sorted-by-popularity list (or vice versa). Storing which fonts have already been requested means we can avoid requesting the same one again.

    -- RequestedFonts also records which fonts were requested together (multiple fonts can be requested per HTTP request). Each list of fonts goes to make up the HTTP request to request that list. If the HTTP request changes, then the DOM changes (because we're using link hrefs to request fonts), and if the DOM changes, the browser might re-request unnecessarily. Sure, a link with the same href might be served by the browser's cache, but that shouldn't be relied upon. Also, while working with link href, we'll have to assume that all requests are successful.
    {-
       <link href="...FontA|FontB|FontC">
       <link href="...FontD|FontE">
    -}
    -- This detail is hidden behind the RequestedFonts type.
    -- rename to fontRequests?
    , mjrNav : MjrNav.Model
    , showAllOrResults : View -- necessary so that the view can choose between using searchResults or model.availableFonts (all) as a data source
    , scrollPosition : Float
    }


type View
    = All
    | SearchResults Fonts -- fonts that match the last search


init : Int -> ( Model, Cmd Msg )
init windowWidth =
    ( { main = Loading
      , windowWidth = windowWidth
      }
    , getAvailableFonts
    )


getAvailableFonts : Cmd Msg
getAvailableFonts =
    Http.get
        { url = developerApiUrl
        , expect =
            expectJson
                (RemoteData.fromResult >> FontsResponse)
                Fonts.decodeFonts
        }


model_Init : Fonts -> Model_
model_Init fonts =
    { availableFonts = LoadedAndUnloadedFonts fontsPerRequest fonts
    , requestedFonts = RequestedFonts.update RequestedFonts.none (Fonts.families fonts)
    , mjrNav = MjrNav.init
    , showAllOrResults = All
    , scrollPosition = 0
    }


developerApiUrl =
    -- "https://www.googleapis.com/webfonts/v1/webfonts?sort=popularity&key=..."
    Url.Builder.crossOrigin
        "https://www.googleapis.com"
        [ "webfonts", "v1", "webfonts" ]
        [ string "sort" "popularity", string "key" apiKey ]



-- PORTS
-- Browser.Dom.getViewport has an issue (https://github.com/elm/browser/issues/118). These ports are a stand-in.


port getViewport : () -> Cmd msg


port viewport : (Viewport -> msg) -> Sub msg


type alias Viewport =
    { sceneHeight : Float
    , viewportHeight : Float
    , viewportY : Float
    }



-- UPDATE


type Msg
    = FontsResponse (WebData Fonts)
    | GotViewport Viewport
    | MjrNav MjrNav.Msg
    | BackToTop
    | WindowResize Int Int
    | NoOp -- for Browser.Dom.setViewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width _ ->
            ( { model | windowWidth = width }, Cmd.none )

        _ ->
            case model.main of
                Success model_ ->
                    let
                        ( main_, cmd ) =
                            updateModel_ msg model_
                    in
                    ( { model | main = Success main_ }, cmd )

                _ ->
                    let
                        ( main_, cmd ) =
                            updateNotSuccess msg model.main
                    in
                    ( { model | main = main_ }, cmd )


updateModel_ : Msg -> Model_ -> ( Model_, Cmd Msg )
updateModel_ msg model =
    case msg of
        GotViewport { sceneHeight, viewportHeight, viewportY } ->
            case model.showAllOrResults of
                All ->
                    -- if we're at the bottom of the page, request some more fonts
                    if viewportY + viewportHeight >= sceneHeight then
                        -- at bottom of page
                        let
                            unloadedFonts =
                                LUFonts.unloaded model.availableFonts

                            fontsToRequest =
                                (Fonts.first fontsPerRequest >> Fonts.families) unloadedFonts

                            fonts_ =
                                LUFonts.load fontsPerRequest model.availableFonts
                        in
                        ( { model
                            | availableFonts = fonts_
                            , requestedFonts = RequestedFonts.update model.requestedFonts fontsToRequest
                            , scrollPosition = viewportY
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | scrollPosition = viewportY }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        MjrNav mjrNavMsg ->
            updateMjrNavMsg mjrNavMsg model

        BackToTop ->
            let
                resetViewport : Cmd Msg
                resetViewport =
                    Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 0)
            in
            ( model, resetViewport )

        NoOp ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateNotSuccess : Msg -> WebData Model_ -> ( WebData Model_, Cmd Msg )
updateNotSuccess msg model =
    case msg of
        FontsResponse response ->
            -- TODO use RemoteData.update here?
            case response of
                Success fonts ->
                    ( Success (model_Init fonts), getViewport () )

                _ ->
                    ( RemoteData.map model_Init response, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateMjrNavMsg : MjrNav.Msg -> Model_ -> ( Model_, Cmd Msg )
updateMjrNavMsg msg model =
    case msg of
        MjrNav.SearchInput input ->
            ( { model
                | mjrNav =
                    MjrNav.update msg model.mjrNav
                , showAllOrResults =
                    case input of
                        "" ->
                            All

                        _ ->
                            model.showAllOrResults
              }
            , Cmd.none
            )

        MjrNav.Search search ->
            -- NOTE not sure whether Search is best as Main.Search or MjrNav.Search. All the updates here are to Main.model, not MjrNav.model, so it feels like it'd make more sense as Main.Search. But Main.Search can't be passed into MjrNav.view as MjrNav.view returns Html MjrNav.Msg. Perhaps Main.Search could be passed to a function that returns Html Main.Msg (probably defined in Main) that also contains MjrNav.view by using Html.map?
            let
                allFonts : Fonts
                allFonts =
                    LUFonts.all model.availableFonts

                searchResults =
                    Fonts.search search allFonts

                fontFamilies =
                    Fonts.families searchResults
            in
            ( { model
                | requestedFonts = RequestedFonts.update model.requestedFonts fontFamilies
                , showAllOrResults = SearchResults searchResults
              }
            , Cmd.none
            )

        MjrNav.Reset ->
            ( { model
                | showAllOrResults = All
                , mjrNav = MjrNav.update msg model.mjrNav
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | mjrNav =
                    MjrNav.update msg model.mjrNav
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ viewport GotViewport
        , Browser.Events.onResize WindowResize
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Favorite Fonts"
    , body =
        [ div [ style "font-family" "sans-serif" ]
            [ Header.responsive model.windowWidth
            , case model.main of
                NotAsked ->
                    text "Initialising..."

                Loading ->
                    text "Fetching up-to-date fonts..."

                RemoteData.Failure err ->
                    text ("Error: " ++ Debug.toString err)

                Success model_ ->
                    loadedView model.windowWidth model_
            ]
        ]
    }


loadedView : Int -> Model_ -> Html Msg
loadedView windowWidth { availableFonts, requestedFonts, mjrNav, scrollPosition, showAllOrResults } =
    div []
        [ RequestedFonts.stylesheetLinks requestedFonts
        , Html.main_ [ style "margin-bottom" "1.5em" ]
            (let
                mainChildren =
                    [ Html.map MjrNav (MjrNav.view mjrNav windowWidth)
                    , fontsView availableFonts showAllOrResults mjrNav
                    ]
             in
             if scrollPosition >= 300 then
                mainChildren ++ [ backToTopButton ]

             else
                mainChildren
            )
        , footer
        ]


fontsView fonts showAllOrResults mjrNav =
    case showAllOrResults of
        All ->
            Fonts.view
                (LUFonts.loaded fonts)
                (sampleText (MjrNav.sampleTextInput mjrNav))
                (MjrNav.fontSize mjrNav)

        SearchResults searchResults ->
            Fonts.view searchResults
                (sampleText (MjrNav.sampleTextInput mjrNav))
                (MjrNav.fontSize mjrNav)


sampleText input =
    if input == "" then
        defaultSampleText

    else
        input


backToTopButton =
    button
        [ style "position" "fixed"
        , style "bottom" "5em"
        , style "right" "1.5em"
        , onClick BackToTop
        ]
        [ text "Back to top" ]


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
        [ div
            [ style "text-align" "center" ]
            [ text "Made by Will White" ]
        ]



-- MAIN


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
