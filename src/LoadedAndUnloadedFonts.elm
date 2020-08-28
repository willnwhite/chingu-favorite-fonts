module LoadedAndUnloadedFonts exposing (..)

-- import Font exposing (Font)

import Fonts exposing (Fonts)



-- "Loaded And Unloaded" refers to whether the font should be visible ("loaded") on the page or not, not whether the font's stylesheet has been requested/loaded.


type LoadedAndUnloadedFonts
    = LoadedAndUnloadedFonts Int Fonts -- Int represents how many fonts are loaded


loaded : LoadedAndUnloadedFonts -> Fonts
loaded (LoadedAndUnloadedFonts n fonts) =
    Fonts.first n fonts


unloaded : LoadedAndUnloadedFonts -> Fonts
unloaded (LoadedAndUnloadedFonts n fonts) =
    Fonts.rest n fonts


all : LoadedAndUnloadedFonts -> Fonts
all (LoadedAndUnloadedFonts _ fonts) =
    fonts


none : LoadedAndUnloadedFonts
none =
    LoadedAndUnloadedFonts 0 Fonts.none


load n (LoadedAndUnloadedFonts l fonts) =
    LoadedAndUnloadedFonts (l + n) fonts
