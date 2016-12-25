module Main exposing (..)

import LevelSequence exposing (..)
import RunLevel exposing (..)
import Html exposing (program)
import Utils exposing (..)
import Random exposing (..)


main =
    let
        (Component c) =
            mkComponent (levelApi (initialSeed 23))
    in
        program c
