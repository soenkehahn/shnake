module Levels exposing (Level, all)

import Position exposing (..)
import Random exposing (..)
import Utils exposing (..)


type alias Level =
    { size : Int
    , player : Position
    , food : List Position
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
                        (\numberOfFood ->
                            Random.map2
                                (\player food ->
                                    { size = size, player = player, food = food }
                                )
                                (position size)
                                (list numberOfFood (position size))
                        )
            )


position : Int -> Generator Position
position size =
    let
        gen =
            int 0 (size - 1)
    in
        Random.map2 Position gen gen
