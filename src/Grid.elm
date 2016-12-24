module Grid exposing (..)

import Array exposing (..)
import Debug exposing (..)
import Position exposing (..)
import Random exposing (..)


type Grid a
    = Grid (Array (Array a))


create : Int -> a -> Grid a
create size a =
    Grid (Array.repeat size (Array.repeat size a))


offset : Int -> Int
offset size =
    floor (toFloat size / 2)


set : Position -> a -> Grid a -> Grid a
set { x, y } a (Grid array) =
    let
        i =
            x + offset (length array)

        j =
            y + offset (length array)
    in
        Grid
            <| case Array.get j array of
                Nothing ->
                    array

                Just row ->
                    Array.set j (Array.set i a row) array


setCells : List Position -> a -> Grid a -> Grid a
setCells positions cell grid =
    List.foldl (\position -> set position cell) grid positions


get : Position -> Grid a -> Maybe a
get { x, y } (Grid array) =
    case Array.get (y + offset (length array)) array of
        Nothing ->
            Nothing

        Just row ->
            Array.get (x + offset (length array)) row


toLists : Grid a -> List (List a)
toLists (Grid x) =
    List.map toList (toList x)


size : Grid a -> Int
size (Grid array) =
    length array


randomPosition : Seed -> Int -> Position -> Position
randomPosition seed1 size player =
    let
        o =
            offset size

        ( x, seed2 ) =
            step (int -o o) seed1

        ( y, seed3 ) =
            step (int -o o) seed2

        result =
            Position x y
    in
        if result == player then
            randomPosition seed3 size player
        else
            result
