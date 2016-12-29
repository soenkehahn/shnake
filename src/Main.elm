module Main exposing (..)

import LevelSequence exposing (..)
import RunLevel exposing (..)
import Html exposing (program)
import Utils exposing (..)
import Level.Model
import Levels
import Level.Definitions


type alias Model =
    LevelSequence.Model Level.Model.Level Level.Model.Model RunLevel.Msg


main : Program Never Model (LevelSequence.Msg RunLevel.Msg)
main =
    let
        (Component c) =
            mkComponent <| levelApi Levels.all
    in
        program c


foo : Int -> Maybe Level.Model.Level
foo =
    Level.Definitions.levels
