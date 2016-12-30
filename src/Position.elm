module Position exposing (..)

import Debug exposing (..)
import Random exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


directionGen : Generator Direction
directionGen =
    int 1 4
        |> map
            (\which ->
                case which of
                    1 ->
                        Up

                    2 ->
                        Down

                    3 ->
                        Left

                    4 ->
                        Right

                    _ ->
                        crash "directionGen: shouldn't happen"
            )


mutateDirection : Direction -> Generator Direction
mutateDirection _ =
    directionGen
