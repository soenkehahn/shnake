module Levels exposing (Level, all)

import Position exposing (..)
import Random exposing (..)
import Utils exposing (..)


type alias Level =
    { size : Int
    , player : Position
    , food : List Position
    , walls : List Position
    }


all : List Level
all =
    fst <| step (list 50 level) (initialSeed 40)


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
                                (position size)
                                (list numberOfObjects (position size))
                                (list (round (toFloat numberOfObjects * 0.1))
                                    (position size)
                                )
                        )
            )


position : Int -> Generator Position
position size =
    let
        gen =
            int 0 (size - 1)
    in
        Random.map2 Position gen gen
