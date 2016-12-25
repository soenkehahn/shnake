module Main exposing (..)

import LevelSequence exposing (..)
import RunLevel exposing (..)
import Html exposing (program)
import Utils exposing (..)


-- main : Program Never (LevelSequence RunLevel.Model RunLevel.Msg
main =
    let
        (Component c) =
            mkComponent (levelApi ())
    in
        program c
