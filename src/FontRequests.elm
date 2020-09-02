module FontRequests exposing (FontRequests, init, stylesheetLinks, update)

import Font exposing (Font, FontFamily)
import Fonts exposing (Fonts)
import Html exposing (..)
import Html.Attributes exposing (..)
import ListExcept


type FontRequests
    = FontRequests (List (List FontFamily))



-- Each list is for one HTTP request. preserving the HTTP requests means the <link>'s hrefs are preserved, thus the DOM won't change and therefore re-requests won't be made


init : Fonts -> FontRequests
init fonts =
    FontRequests [ Fonts.map Font.family fonts ]


update : FontRequests -> Fonts -> FontRequests
update (FontRequests fontRequests) fonts =
    let
        fontFamilies =
            Fonts.map Font.family fonts

        fontFamiliesExcept =
            ListExcept.except (List.concat fontRequests) fontFamilies
    in
    case fontFamiliesExcept of
        [] ->
            FontRequests fontRequests

        x ->
            FontRequests (fontRequests ++ [ x ])



-- explain why this is necessary, all the way back to the links
-- filter out fonts needed that have already been requested.
-- test: when there are no fonts to request over what's already been requested, just return the original fontsForLinks, not fontsForLinks with empty lists in it.
-- > import Main
-- > Main.fontsToRequest ["a","b"] ["a"]
-- Nothing : Maybe (List String)
-- > Main.fontsToRequest ["a","b"] ["c"]
-- Just ["c"] : Maybe (List String)
-- (write a test: when there are fonts to request over what's already been requested, only the fonts that haven't already been requested will be returned.)
-- VIEW


stylesheetLinks : FontRequests -> Html msg
stylesheetLinks (FontRequests fontRequests) =
    div [] (List.map stylesheetLink fontRequests)


stylesheetLink : List FontFamily -> Html msg
stylesheetLink fontFamilies =
    -- <link rel="stylesheet" href="...">
    Html.node "link" [ rel "stylesheet", href (fontRequestUrl fontFamilies) ] []


fontRequestUrl fontFamilies =
    "https://fonts.googleapis.com/css?family="
        -- no percent encoding, else font requested is e.g. "Open+Sans" not "Open Sans"
        ++ familyUrlParameter fontFamilies


familyUrlParameter : List FontFamily -> String
familyUrlParameter =
    -- ["Open Sans","Roboto"] -> "Open+Sans|Roboto"
    List.map (String.replace " " "+") >> String.join "|"
