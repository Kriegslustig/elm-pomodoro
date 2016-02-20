module PomodoroBackground (Model, init, Action(..), update, view) where

import Signal exposing (Address)
import Html exposing (Html, div)
import Html.Attributes exposing (style)

type alias Model =
  { state : Float
  , max : Float
  , pause : Bool
  }

init : Int -> Model
init max =
  { state = toFloat 0
  , max = toFloat max
  , pause = True
  }

type Action = Nope
  | Increment
  | Decrement

update : Action -> Model -> Model
update action model =
  case action of
    Nope ->
      model

    Increment ->
      { model |
        state = model.state+1.0
      }

    Decrement ->
      { model |
        state = model.state-1.0
      }

calcHeight : Float -> Float -> String
calcHeight state max =
  let p = state/max*100
  in toString p ++ "%"

view : Address Action -> Model -> Html
view address model =
  div
    [ style
      [ ("position", "absolute")
      , ("bottom", "0")
      , ("left", "0")
      , ("width", "100%")
      , if model.pause
        then ("background", "hsl(80, 30%, 80%)")
        else ("background", "hsl(10, 30%, 80%)")
      , ("z-index", "-1")
      , ("height", calcHeight model.state model.max)
      ]
    ]
    []

