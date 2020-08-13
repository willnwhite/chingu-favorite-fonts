module Fonts exposing (..)

import Json.Decode exposing (..)

type alias Fonts = List Font

type alias Font =
  { family : String
  , category : String
  }


decodeFonts : Decoder Fonts
decodeFonts =
    at ["items"] (list decodeFont)

decodeFont : Decoder Font
decodeFont =
  map2 Font
    (field "family" string)
    (field "category" string)
  -- an object with "family" and "category" fields
