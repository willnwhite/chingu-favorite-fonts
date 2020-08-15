module Fonts exposing (Fonts, Font, decodeFonts)

import Json.Decode exposing (..)

type alias Fonts = List Font

type alias Font =
  { family : String
  , category : String
  }



-- DECODERS

-- JSON structure to decode:
-- {"items": [{ "family": "Roboto", "category": "sans-serif", ... }], ...}

decodeFonts : Decoder Fonts
decodeFonts =
    at ["items"] (list decodeFont)

decodeFont : Decoder Font
decodeFont =
  map2 Font
    (field "family" string)
    (field "category" string)
