module Level.Generation exposing (..)

import Debug exposing (..)
import Stream exposing (..)
import Level.Model exposing (..)
import Random exposing (..)
import Position exposing (..)


randomLevels : Seed -> Stream Level
randomLevels seed =
    randomStream seed randomLevel


randomLevel : Generator Level
randomLevel =
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
