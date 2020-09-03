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
    { main : WebData SuccessModel
    , windowWidth : Int
    }


type alias SuccessModel =
    { availableFonts : Fonts -- the fonts that are available from Google Fonts, sorted by popularity. used to calculate fontsToShow and search results
    , visibleFonts : Fonts -- the fonts shown in the All view (doesn't include fonts from searches)
    , fontRequests : FontRequests -- the font families used to build each request for fonts:

    {-
       <link href="...FontA|FontB|FontC">
       <link href="...FontD|FontE">
    -}
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
            expectJson (RemoteData.fromResult >> FontsResponse) Fonts.decoder
        }


successModelInit : Fonts -> SuccessModel
successModelInit fonts =
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
                            updateSuccessModel msg model_
                    in
                    ( { model | main = Success main_ }, cmd )

                _ ->
                    let
                        ( main_, cmd ) =
                            updateNotSuccess msg model.main
                    in
                    ( { model | main = main_ }, cmd )


updateSuccessModel : Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateSuccessModel msg model =
    case msg of
        GotViewport { sceneHeight, viewportHeight, viewportY } ->
            case model.showAllOrResults of
                All ->
                    -- if we're at the bottom of the page, request some more fonts
                    if viewportY + viewportHeight >= sceneHeight then
                        -- at bottom of page
                        let
                            unloadedFonts =
                                Fonts.except model.visibleFonts model.availableFonts

                            fontsToMakeVisible =
                                Fonts.take fontsPerRequest unloadedFonts
                        in
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


updateNotSuccess : Msg -> WebData SuccessModel -> ( WebData SuccessModel, Cmd Msg )
updateNotSuccess msg model =
    case msg of
        FontsResponse response ->
            ( RemoteData.map successModelInit response
            , case response of
                Success fonts ->
                    getViewport ()

                _ ->
                    Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateMjrNavMsg : MjrNav.Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
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
            let
                searchResults =
                    Fonts.search search model.availableFonts

                fontFamilies =
                    Fonts.map Font.family searchResults
            in
            ( { model
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


successView : Int -> SuccessModel -> Html Msg
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
fontsView visibleFonts showAllOrResults fontSize sampleText_ =
    let
        fonts =
            case showAllOrResults of
                All ->
                    visibleFonts

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
