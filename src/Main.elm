port module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Font
import FontRequests exposing (FontRequests)
import Fonts exposing (Fonts)
import Header
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..), expectJson)
import MajorNavigation as MjrNav
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Url.Builder exposing (string)



-- CONFIGURATION


apiKey =
    "AIzaSyDXdgHuIP_D5ySRE5oA-Hd2qoZaaDBPCO4"


fontsPerRequest =
    8


defaultSampleText =
    "Making the Web Beautiful!"


defaultFontSize =
    "32px"



-- MODEL


type alias Model =
    { main : WebData Model_
    , windowWidth : Int
    }


type alias Model_ =
    { availableFonts : Fonts -- the fonts that are available from Google Fonts, sorted by popularity. used to calculate fontsToShow and search results
    , visibleFonts : Fonts -- the fonts shown in the All view (doesn't include fonts from searches)
    , fontRequests : FontRequests -- The same font should not be requested more than once (via link href or however). This could happen if a font is in a search result but it's already in the sorted-by-popularity list (or vice versa). Storing which fonts have already been requested means we can avoid requesting the same one again. (includes fonts from searches)

    -- FontRequests also records which fonts were requested together (multiple fonts can be requested per HTTP request). Each list of fonts goes to make up the HTTP request to request that list. If the HTTP request changes, then the DOM changes (because we're using link hrefs to request fonts), and if the DOM changes, the browser might re-request unnecessarily. Sure, a link with the same href might be served by the browser's cache, but that shouldn't be relied upon. Also, while working with link href, we'll have to assume that all requests are successful.
    {-
       <link href="...FontA|FontB|FontC">
       <link href="...FontD|FontE">
    -}
    -- This detail is hidden behind the FontRequests type.
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
                Fonts.decoder
        }



-- model_Init : Fonts -> Model_
-- model_Init fonts =
--     let
--         availableFonts =
--             LoadedAndUnloadedFonts fontsPerRequest fonts
--
--         fontsToRequest =
--             LUFonts.loaded availableFonts |> Fonts.families
--     in
--     { availableFonts = availableFonts
--     , fontRequests = FontRequests.update FontRequests.none fontsToRequest
--     , mjrNav = MjrNav.init defaultFontSize
--     , showAllOrResults = All
--     , scrollPosition = 0
--     }


model_Init : Fonts -> Model_
model_Init fonts =
    let
        fontsToMakeVisible =
            Fonts.take fontsPerRequest fonts
    in
    { availableFonts = fonts
    , visibleFonts = fontsToMakeVisible
    , fontRequests = FontRequests.init fontsToMakeVisible
    , mjrNav = MjrNav.init defaultFontSize
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
                                -- LUFonts.unloaded model.availableFonts
                                Fonts.except model.visibleFonts model.availableFonts

                            fontsToMakeVisible =
                                Fonts.take fontsPerRequest unloadedFonts

                            -- fontsToRequest =
                            --     (Fonts.take fontsPerRequest >> Fonts.families) unloadedFonts
                            -- fonts_ =
                            --     LUFonts.load fontsPerRequest model.availableFonts
                        in
                        -- ( { model
                        --     | availableFonts = fonts_
                        --     , fontRequests = FontRequests.update model.fontRequests fontsToRequest
                        --     , scrollPosition = viewportY
                        --   }
                        -- , Cmd.none
                        -- )
                        ( { model
                            | visibleFonts = Fonts.append model.visibleFonts fontsToMakeVisible
                            , fontRequests = FontRequests.update model.fontRequests fontsToMakeVisible
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
                -- allFonts : Fonts
                -- allFonts =
                --     LUFonts.all model.availableFonts
                searchResults =
                    -- Fonts.search search allFonts
                    Fonts.search search model.availableFonts

                fontFamilies =
                    -- Fonts.families searchResults
                    Fonts.map Font.family searchResults
            in
            ( { model
                -- | fontRequests = FontRequests.update model.fontRequests fontFamilies
                | fontRequests = FontRequests.update model.fontRequests searchResults
                , showAllOrResults = SearchResults searchResults
              }
            , Cmd.none
            )

        MjrNav.Reset ->
            ( { model
                | showAllOrResults = All
                , mjrNav = MjrNav.init defaultFontSize
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
                    failureView err

                Success model_ ->
                    successView model.windowWidth model_
            ]
        ]
    }


failureView : Http.Error -> Html msg
failureView err =
    case err of
        BadUrl url ->
            text ("Google Fonts couldn't understand this website's request. The request was: " ++ url)

        Timeout ->
            text "It took too long to get a response from Google Fonts."

        NetworkError ->
            text "Your network connection failed."

        BadStatus code ->
            text ("Failure: status " ++ String.fromInt code)

        BadBody responseBody ->
            text ("This website couldn't understand Google Fonts' response. The response was: " ++ responseBody)


successView : Int -> Model_ -> Html Msg
successView windowWidth { availableFonts, visibleFonts, fontRequests, mjrNav, scrollPosition, showAllOrResults } =
    div []
        [ FontRequests.stylesheetLinks fontRequests
        , Html.main_ [ style "margin-bottom" "1.5em" ]
            (let
                mainChildren =
                    [ Html.map MjrNav (MjrNav.view mjrNav windowWidth)
                    , fontsView
                        visibleFonts
                        showAllOrResults
                        (MjrNav.fontSize mjrNav)
                        (sampleText (MjrNav.sampleTextInput mjrNav))
                    ]
             in
             if scrollPosition >= 300 then
                mainChildren ++ [ backToTopButton ]

             else
                mainChildren
            )
        , footer
        ]


fontsView : Fonts -> View -> String -> String -> Html msg
fontsView availableFonts showAllOrResults fontSize sampleText_ =
    let
        fonts =
            case showAllOrResults of
                All ->
                    availableFonts

                SearchResults searchResults ->
                    searchResults
    in
    Fonts.view fonts sampleText_ fontSize


sampleText : String -> String
sampleText input =
    if input == "" then
        defaultSampleText

    else
        input


backToTopButton : Html Msg
backToTopButton =
    button
        [ style "position" "fixed"
        , style "bottom" "5em"
        , style "right" "1.5em"
        , onClick BackToTop
        ]
        [ text "Back to top" ]


footer : Html msg
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
