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
import Dict
import Level.Generated


all : Int -> Maybe Level
all n =
    let
        levelMap =
            Dict.fromList
                <| Level.Generated.levels
    in
        case Dict.get n levelMap of
            Just x ->
                Just x

            Nothing ->
                Just <| fst <| step randomLevel (initialSeed (n * 1234))
