module FontRequests exposing (..)

import Font exposing (FontFamily)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias FontRequests =
    List (List FontFamily)


none : FontRequests
none =
    []



-- Each list is for one HTTP request. preserving the HTTP requests means the <link>'s hrefs are preserved, thus the DOM won't change and therefore re-requests won't be made


update : FontRequests -> List FontFamily -> FontRequests
update fontRequests fontsNeeded =
    let
        filterOutList : List a -> List a -> List a
        filterOutList fontRequests_ =
            List.filter (\result -> not (List.member result fontRequests_))

        -- TODO name parameters (or go point-free), put function at top-level to be tested, then PR to List.Extra
    in
    case filterOutList (List.concat fontRequests) fontsNeeded of
        [] ->
            fontRequests

        newFonts ->
            fontRequests ++ [ newFonts ]



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


stylesheetLinks fontRequests =
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
