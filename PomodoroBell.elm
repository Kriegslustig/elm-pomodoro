module PomodoroBell (Action(..), update, view, init, Model) where

import Html exposing (Html, audio)
import Html.Attributes exposing (src, controls)
import Signal exposing (Address)
import Time

type alias Model =
  { state : Int
  , url : String
  }

type Action = NoOp
  | StartAudio

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    StartAudio ->
      { model |
        state = model.state+1
      }

init : String -> Model
init url =
  { state = 0
  , url = url
  }

view : Address Action -> Model -> Html
view address model =
  audio
    [ src model.url ]
    []

