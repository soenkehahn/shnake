module Level.Definitions exposing (..)

import Position exposing (..)
import Level.Model exposing (..)
import Level.Solution exposing (..)


levels : Int -> Maybe Level
levels n =
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

        _ ->
            Nothing
