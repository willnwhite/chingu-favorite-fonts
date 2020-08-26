module Fonts exposing (Font, decodeFonts)

import Json.Decode exposing (..)

type alias Font =
  { family : String
  , category : String
  }



-- DECODERS

-- JSON structure to decode:
-- {"items": [{ "family": "Roboto", "category": "sans-serif", ... }], ...}

decodeFonts : Decoder (List Font)
decodeFonts =
    at ["items"] (list decodeFont)

decodeFont : Decoder Font
decodeFont =
  map2 Font
    (field "family" string)
    (field "category" string)
