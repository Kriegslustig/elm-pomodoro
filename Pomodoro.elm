module Pomodoro where

import Html exposing (Html, text, p, div, button)
import Html.Events exposing (onClick)
import Signal exposing (Mailbox, mailbox, Address, sampleOn, constant, merge)
import List exposing (head, take, length, reverse)
import Maybe exposing (withDefault)
import Time exposing (every, second)

type alias Model =
  { time : Int
  , round : Int
  , pause : Bool
  , running : Bool
  , length : Int
  }

type Action = Nope
  | IncrementSeconds
  | NewRound
  | ToggleRunning
  | Reset

cycle : List (Int)
cycle =
  [ 25 * 60
  , 5 * 60
  , 25 * 60
  , 5 * 60
  , 25 * 60
  , 5 * 60
  , 25 * 60
  , 10 * 60
  ]

runningToggleValue : Bool -> String
runningToggleValue running =
  if running
     then "Pause"
     else "Start"

incTime : Signal Action
incTime =
  sampleOn
    (every second)
    (constant IncrementSeconds)

update : Action -> Model -> Model
update action model =
  case action of
    Nope ->
      model

    IncrementSeconds ->
      if model.time >= model.length
        then { model |
            time = 0
          , round = model.round+1
          , pause = not model.pause
          , length = withDefault 0
            <| head
            <| reverse
            <| take ((model.round+1) % length cycle) cycle
          }
        else if model.running
        then { model |
            time = model.time+1
          }
        else
          model

    NewRound ->
      { model |
        time = model.length
      }

    ToggleRunning ->
      { model |
        running = not model.running
      }

    Reset ->
      init

view : Address Action -> Model -> Html
view address model =
  div []
    [ p []
      [ text <| "Time: " ++ toString model.time ]
    , p []
      [ text <| "Round: " ++ toString model.round ]
    , button
      [ onClick address ToggleRunning ]
      [ text <| runningToggleValue model.running ]
    , button
      [ onClick address Reset ]
      [ text "Reset" ]
    , button
      [ onClick address NewRound ]
      [ text "Skip" ]
    ]

init : Model
init =
  { time = 0
  , round = 0
  , pause = True
  , running = True
  , length = 0
  }

actions : Mailbox Action
actions =
  mailbox Nope

model : Signal Action -> Signal Model
model signal =
  Signal.foldp update init signal

main : Signal Html
main =
  Signal.map
    (view actions.address)
    <| model (merge actions.signal incTime)

