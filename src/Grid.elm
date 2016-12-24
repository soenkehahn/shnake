module Grid exposing (..)

import Array exposing (..)
import Debug exposing (..)
import Position exposing (..)
import Random exposing (..)


type Grid a
    = Grid (Array (Array a))



-- fixme: -> newGrid


create : Int -> a -> Grid a
create size a =
    Grid (Array.repeat size (Array.repeat size a))


inGrid : Int -> Position -> Bool
inGrid size position =
    (position.x < size)
        && (position.x >= 0)
        && (position.y < size)
        && (position.y >= 0)


setCell : Position -> a -> Grid a -> Grid a
setCell { x, y } a (Grid array) =
    Grid
        <| case Array.get y array of
            Nothing ->
                array

            Just row ->
                Array.set y (Array.set x a row) array


setCells : List Position -> a -> Grid a -> Grid a
setCells positions cell grid =
    List.foldl (\position -> setCell position cell) grid positions


get : Position -> Grid a -> Maybe a
get { x, y } (Grid array) =
    case Array.get y array of
        Nothing ->
            Nothing

        Just row ->
            Array.get x row


toLists : Grid a -> List (List a)
toLists (Grid x) =
    List.map toList (toList x)


size : Grid a -> Int
size (Grid array) =
    length array


randomPosition : Seed -> Int -> Position -> ( Position, Seed )
randomPosition seed1 size player =
    let
        ( x, seed2 ) =
            step (int 0 (size - 1)) seed1

        ( y, seed3 ) =
            step (int 0 (size - 1)) seed2

        result =
            Position x y
    in
        if result == player then
            randomPosition seed3 size player
        else
            ( result, seed3 )
