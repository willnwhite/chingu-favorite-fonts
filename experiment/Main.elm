port module Main exposing (..)

import Browser
import Browser.Dom
import Task
import Html
import Html.Attributes


-- INIT

init : () -> ((), Cmd msg)
init _ = ((), Cmd.none)

-- PORTS

port scroll : ({ scrollHeight : Float, clientHeight : Float } -> msg) -> Sub msg



-- UPDATE

type Msg =
  Scroll { scrollHeight : Float, clientHeight : Float }
  -- | ViewportOf (Result Browser.Dom.Error Browser.Dom.Viewport)
  | Viewport Browser.Dom.Viewport

update msg model =
  case msg of
    -- ViewportOf result ->
    --   case result of
    --     Ok v ->
    --       let
    --           _ = Debug.log "getViewportOf" v
    --       in
    --           (model, Cmd.none)
    --     Err error ->
    --       let
    --           _ = Debug.log "error" error
    --       in
    --           (model, Cmd.none)

    Viewport v ->
      let
          _ = Debug.log "getViewport" v
      in
          (model, Cmd.none)

    Scroll heights ->
      let
          _ = Debug.log "heights" heights
      in

      ( model, Cmd.batch
        [ {-Task.attempt ViewportOf (Browser.Dom.getViewportOf "scroll_")
        , -}Task.perform Viewport Browser.Dom.getViewport
        ] )



-- VIEW

view model =
  { title = "SSCEE"
  , body = [ Html.div [{-Html.Attributes.id "scroll"-}] (List.map (\n -> Html.div [] [Html.text (String.fromInt n)]) (List.range 1 100)) ]
  }

-- SUBSCRIPTIONS

subscriptions model =
  scroll Scroll



-- MAIN

main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
