module Levels exposing (..)

import Position exposing (..)
import Random exposing (..)
import Utils exposing (..)
import Debug exposing (..)
import Player exposing (..)
import Level.Model exposing (..)
import Stream exposing (..)


all : Int -> Maybe Level
all n =
    case n of
        0 ->
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

        1 ->
            Just <| fst <| step level (initialSeed 23)

        _ ->
            Nothing


level : Generator Level
level =
    int 3 11
        |> andThen
            (\size ->
                int 1 (size ^ 2)
                    |> andThen
                        (\numberOfObjects ->
                            Random.map3
                                (\player food walls ->
                                    { size = size
                                    , player = player
                                    , food = food
                                    , walls = walls
                                    }
                                )
                                (positionGen size)
                                (list numberOfObjects (positionGen size))
                                (list (round (toFloat numberOfObjects * 0.1))
                                    (positionGen size)
                                )
                        )
            )


positionGen : Int -> Generator Position
positionGen size =
    let
        gen =
            int 0 (size - 1)
    in
        Random.map2 Position gen gen
