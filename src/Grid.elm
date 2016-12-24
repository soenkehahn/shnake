module Grid exposing (..)

import Array exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type Grid a
    = Grid (Array (Array a))


create : a -> Grid a
create a =
    Grid (Array.repeat 21 (Array.repeat 21 a))


set2 : Position -> a -> Grid a -> Grid a
set2 { x, y } a (Grid array) =
    let
        i =
            x + 10

        j =
            y + 10
    in
        Grid
            <| case get j array of
                Nothing ->
                    array

                Just row ->
                    set j (set i a row) array


get2 : Position -> Grid a -> Maybe a
get2 { x, y } (Grid array) =
    case get y array of
        Nothing ->
            Nothing

        Just row ->
            get x row
