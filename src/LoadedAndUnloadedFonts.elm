module LoadedAndUnloadedFonts exposing (..)

import Font exposing (Font)



-- "Loaded And Unloaded" refers to whether the font should be visible ("loaded") on the page or not, not whether the font's stylesheet has been requested/loaded.


type LoadedAndUnloadedFonts
    = LoadedAndUnloadedFonts Int (List Font) -- Int represents how many fonts are loaded


loaded : LoadedAndUnloadedFonts -> List Font
loaded (LoadedAndUnloadedFonts n fonts) =
    List.take n fonts


unloaded (LoadedAndUnloadedFonts n fonts) =
    List.drop n fonts


load n (LoadedAndUnloadedFonts l fonts) =
    LoadedAndUnloadedFonts (l + n) fonts
