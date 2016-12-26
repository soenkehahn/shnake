module Levels exposing (..)

import Debug exposing (..)
import Level.Generation exposing (..)
import Level.Model exposing (..)
import Level.Solution exposing (..)
import Player exposing (..)
import Position exposing (..)
import Random exposing (..)
import Stream exposing (..)
import Utils exposing (..)
import Level.Definitions exposing (..)


all : Int -> Maybe Level
all n =
    case Level.Definitions.levels n of
        Nothing ->
            Just <| fst <| step randomLevel (initialSeed n)

        Just x ->
            Just x
