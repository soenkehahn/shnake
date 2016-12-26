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


all : Int -> Maybe Level
all n =
    case n of
        0 ->
            Just <| findLevelByStrategy [ Left ]

        1 ->
            Just <| findLevelByStrategy [ Left, Left, Down ]

        2 ->
            Just
                <| Level 5
                    (Position 0 0)
                    [ Position 4 4 ]
                    [ Position 0 1
                    , Position 1 1
                    , Position 2 1
                    , Position 3 3
                    , Position 4 3
                    ]

        n ->
            Just <| fst <| step randomLevel (initialSeed n)
