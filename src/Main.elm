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
import MajorNavigation
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
    -- there's a split between All and SearchResults data:
    -- if you get a font in a search result, that font shouldn't be "thrown in" to the sorted-by-popularity list, even if it's in the right place, because there's no guarantee there won't be fonts missing. Eg. sorted by popularity could show A, B, C, and search could return E. Now if you put E on the end of A, B, C, it looks like E is the next-most popular to C, when in fact that's D. Also, when D is loaded, E will jump down. you do, however, want to keep the links for any fonts the search has requested. that way, if any of the search results do pop up in the popularity list, they'll already have their stylesheets.
    { availableFonts : WebData LoadedAndUnloadedFonts -- what fonts are available, from Google Fonts Developer API, sorted by popularity

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
    , majorNavigation : MajorNavigation.Model
    , showAllOrResults : View -- necessary so that the view can choose between using searchResults or model.availableFonts (all) as a data source
    , windowWidth : Int
    , scrollPosition : Float
    }



-- perhaps refactor so that everything dependent on the list of fonts being fetched successfully is part of fonts -- same for searchResults: it should only exist if there's a search (not "")


type View
    = All
    | SearchResults Fonts -- fonts that match the last search (subset of model.availableFonts)


init : Int -> ( Model, Cmd Msg )
init windowWidth =
    ( { availableFonts = Loading
      , requestedFonts = []
      , showAllOrResults = All
      , majorNavigation = MajorNavigation.init
      , windowWidth = windowWidth
      , scrollPosition = 0
      }
    , getAvailableFonts
    )


getAvailableFonts =
    Http.get
        { url = developerApiUrl
        , expect =
            expectJson
                (RemoteData.fromResult
                    >> RemoteData.map (LoadedAndUnloadedFonts fontsPerRequest)
                    >> FontsResponse
                )
                Fonts.decodeFonts
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
    = FontsResponse (WebData LoadedAndUnloadedFonts)
    | GotViewport Viewport
    | MajorNavigation MajorNavigation.Msg
    | BackToTop
    | WindowResize Int Int
    | NoOp -- for Browser.Dom.setViewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport { sceneHeight, viewportHeight, viewportY } ->
            case model.showAllOrResults of
                All ->
                    -- if we're at the bottom of the page, request some more fonts
                    if viewportY + viewportHeight >= sceneHeight then
                        -- at bottom of page
                        let
                            unloadedFonts =
                                LUFonts.unloaded (RemoteData.withDefault LUFonts.none model.availableFonts)

                            fontsToRequest =
                                (Fonts.first fontsPerRequest >> Fonts.families) unloadedFonts

                            updateFonts fontsToBeUpdated =
                                -- move this to LUFonts?
                                ( LUFonts.load fontsPerRequest fontsToBeUpdated, Cmd.none )

                            ( fonts_, _ ) =
                                RemoteData.update updateFonts model.availableFonts
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

        MajorNavigation msg_ ->
            case msg_ of
                -- TODO each of these has to be enumerated because they set model.showAllOrResults. If there's a way for the view to calculate whether it should show All of SearchResults based on the model, this would be unnecessary...
                MajorNavigation.SearchInput input ->
                    ( { model
                        | majorNavigation =
                            MajorNavigation.update msg_ model.majorNavigation
                        , showAllOrResults =
                            case input of
                                "" ->
                                    All

                                _ ->
                                    model.showAllOrResults
                      }
                    , Cmd.none
                    )

                MajorNavigation.Search search ->
                    -- NOTE not sure whether Search is best as Main.Search or MajorNavigation.Search. All the updates here are to Main.model, not MajorNavigation.model, so it feels like it'd make more sense as Main.Search. But Main.Search can't be passed into MajorNavigation.view if MajorNavigation.view returns Html MajorNavigation.Msg. Perhaps Main.Search could be passed to a function that returns Html Main.Msg (probably defined in Main) that also contains MajorNavigation.view by using Html.map?
                    let
                        allFonts : Fonts
                        -- allFonts =
                        --     LUFonts.all (RemoteData.withDefault LUFonts.none model.availableFonts)
                        allFonts =
                            RemoteData.unwrap [] LUFonts.all model.availableFonts

                        -- TODO Rather than using withDefault/unwrap (risky: you have to be sure you're using them in the right place), factor any data that depends on the fonts being loaded from the API (eg searchResults) into model.availableFonts (WebData x). That way you can use RemoteData.update to update it.
                        searchResults =
                            -- Fonts.search model.searchInput allFonts
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

                MajorNavigation.Reset ->
                    ( { model
                        | showAllOrResults = All
                        , majorNavigation = MajorNavigation.update msg_ model.majorNavigation
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | majorNavigation =
                            MajorNavigation.update msg_ model.majorNavigation
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

        FontsResponse response ->
            -- TODO use RemoteData.update here
            case response of
                Success fonts ->
                    let
                        fontsToRequest =
                            LUFonts.loaded fonts |> Fonts.families
                    in
                    ( { model
                        | availableFonts = response
                        , requestedFonts = RequestedFonts.update model.requestedFonts fontsToRequest
                      }
                    , getViewport ()
                      -- we've loaded fontsPerRequest fonts by this point, but now check whether we're at the bottom of the page or not
                    )

                _ ->
                    ( { model | availableFonts = response }, Cmd.none )



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
        case model.availableFonts of
            NotAsked ->
                [ text "Initialising..." ]

            Loading ->
                [ text "Fetching up-to-date fonts..." ]

            RemoteData.Failure err ->
                [ text ("Error: " ++ Debug.toString err) ]

            Success fonts ->
                [ viewWhenFontsLoaded fonts model ]
    }


viewWhenFontsLoaded : LoadedAndUnloadedFonts -> Model -> Html Msg
viewWhenFontsLoaded fonts ({ windowWidth, requestedFonts, majorNavigation, scrollPosition, showAllOrResults } as model_) =
    -- TODO another example of why factoring these parameters into the WebData type would make sense (too many parameters)
    div [ style "font-family" "sans-serif" ]
        [ Header.wideOrNarrow windowWidth
        , RequestedFonts.stylesheetLinks requestedFonts
        , Html.main_ [ style "margin-bottom" "1.5em" ]
            (let
                mainChildren =
                    [ Html.map MajorNavigation (MajorNavigation.view model_.majorNavigation windowWidth)
                    , fontsView fonts model_
                    ]
             in
             if scrollPosition >= 300 then
                mainChildren ++ [ backToTopButton ]

             else
                mainChildren
            )
        , footer
        ]


fontsView fonts { showAllOrResults, majorNavigation } =
    case showAllOrResults of
        All ->
            Fonts.view
                (LUFonts.loaded fonts)
                (sampleText (MajorNavigation.sampleTextInput majorNavigation))
                (MajorNavigation.fontSize majorNavigation)

        SearchResults searchResults ->
            Fonts.view searchResults
                (sampleText (MajorNavigation.sampleTextInput majorNavigation))
                (MajorNavigation.fontSize majorNavigation)


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
