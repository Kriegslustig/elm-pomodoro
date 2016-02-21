module FormatTime (ftime) where

import Debug
import Time exposing (inSeconds, inMinutes)

ftime : Int -> String
ftime t =
  let
    time = toFloat (t * 1000)
  in
    ( time
      |> inMinutes
      |> round
      |> toString
    ) ++
    ":" ++
    ( time
      |> inSeconds
      |> round
      |> (flip (%)) 60
      |> toString
    )

