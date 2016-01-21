module Pomodoro where

import PomodoroBackground as Bg
import PomodoroBell as Bell

import Html exposing (Html, text, p, div, button)
import Html.Events exposing (onClick)
import Signal exposing (map, Mailbox, mailbox, Address, sampleOn, constant, merge, forwardTo, filter)
import List exposing (head, take, length, reverse)
import Maybe exposing (withDefault)
import Time exposing (every, second)

type alias Model =
  { time : Int
  , round : Int
  , work : Int
  , pause : Bool
  , running : Bool
  , length : Int
  , background: Bg.Model
  , bell: Bell.Model
  }

type Action = Nope
  | IncrementSeconds
  | NewRound
  | ToggleRunning
  | Reset
  | UpdateBg Bg.Action
  | UpdateBell Bell.Action

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

getCycle : List (Int) -> Int -> Int
getCycle cycle round =
  withDefault 0
    <| head
    <| reverse
    <| take ((round+1) % length cycle) cycle

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
          , work = if model.pause then model.work+1 else model.work
          , bell = if model.pause
              then Bell.update Bell.StartAudio model.bell
              else model.bell
          , pause = not model.pause
          , length = getCycle cycle model.round
          , background = Bg.init <| getCycle cycle model.round
          }
        else if model.running
        then { model |
            background = Bg.update Bg.Increment model.background
          , time = model.time+1
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

    UpdateBg action ->
      { model |
        background = Bg.update action model.background
      }

    UpdateBell action ->
      { model |
        bell = Bell.update action model.bell
      }

view : Address Action -> Model -> Html
view address model =
  div []
    [ p []
      [ text <| "Time: " ++ toString model.time ]
    , p []
      [ text <| "Round: " ++ toString model.work ]
    , button
      [ onClick address ToggleRunning ]
      [ text <| runningToggleValue model.running ]
    , button
      [ onClick address Reset ]
      [ text "Reset" ]
    , button
      [ onClick address NewRound ]
      [ text "Skip" ]
    , Bg.view (forwardTo address UpdateBg) model.background
    , Bell.view (forwardTo address UpdateBell) model.bell
    , button
      [ onClick address (UpdateBell Bell.StartAudio) ]
      [ text "Bell" ]
    ]

init : Model
init =
  { time = 0
  , round = 0
  , work = 0
  , pause = True
  , running = True
  , length = 0
  , background = Bg.init 0
  , bell = Bell.init "./static/bell.ogg"
  }

actions : Mailbox Action
actions =
  mailbox Nope

port playSound : Signal Int
port playSound =
  map (\model -> model.bell.state) model

model : Signal Model
model =
  Signal.foldp update init (merge actions.signal incTime)

main : Signal Html
main =
  map
    (view actions.address)
    model

