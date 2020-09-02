module LoadedAndUnloadedFonts exposing (..)

import Fonts exposing (Fonts)



-- "Loaded And Unloaded" refers to whether the font should be visible ("loaded") on the page or not, not whether the font's stylesheet has been requested/loaded.


type LoadedAndUnloadedFonts
    = LoadedAndUnloadedFonts Int Fonts -- Int represents how many fonts are loaded -- TODO refactor to not use Int, rather two separate lists of loaded and unloaded fonts (as that's easier to use and understand, and there's no evidence that that would affect user experience)


loaded : LoadedAndUnloadedFonts -> Fonts
loaded (LoadedAndUnloadedFonts n fonts) =
    Fonts.take n fonts


unloaded : LoadedAndUnloadedFonts -> Fonts
unloaded (LoadedAndUnloadedFonts n fonts) =
    Fonts.drop n fonts


all : LoadedAndUnloadedFonts -> Fonts
all (LoadedAndUnloadedFonts _ fonts) =
    fonts


none : LoadedAndUnloadedFonts
none =
    LoadedAndUnloadedFonts 0 Fonts.none


load n (LoadedAndUnloadedFonts l fonts) =
    LoadedAndUnloadedFonts (l + n) fonts
