module RequestedFonts exposing (RequestedFonts, update)


type alias RequestedFonts =
    List (List FontFamily)



-- Each list is for one HTTP request. preserving the HTTP requests means the <link>'s hrefs are preserved, thus the DOM won't change and therefore re-requests won't be made


type alias FontFamily =
    String


update : RequestedFonts -> List String -> RequestedFonts
update requestedFonts fontsNeeded =
    let
        filterOutList : List a -> List a -> List a
        filterOutList requestedFonts_ =
            List.filter (\result -> not (List.member result requestedFonts_))

        -- TODO name parameters (or go point-free), put function at top-level to be tested, then PR to List.Extra
    in
    case filterOutList (List.concat requestedFonts) fontsNeeded of
        [] ->
            requestedFonts

        newFonts ->
            requestedFonts ++ [ newFonts ]



-- explain why this is necessary, all the way back to the links
-- filter out fonts needed that have already been requested.
-- test: when there are no fonts to request over what's already been requested, just return the original fontsForLinks, not fontsForLinks with empty lists in it.
-- > import Main
-- > Main.fontsToRequest ["a","b"] ["a"]
-- Nothing : Maybe (List String)
-- > Main.fontsToRequest ["a","b"] ["c"]
-- Just ["c"] : Maybe (List String)
-- (write a test: when there are fonts to request over what's already been requested, only the fonts that haven't already been requested will be returned.)
