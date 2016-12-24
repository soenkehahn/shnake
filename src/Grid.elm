module Grid exposing (..)

import Array exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type Grid a
    = Grid (Array (Array a))


create : Int -> a -> Grid a
create size a =
    Grid (Array.repeat size (Array.repeat size a))


offset (Grid array) =
    floor (toFloat (length array) / 2)


set2 : Position -> a -> Grid a -> Grid a
set2 { x, y } a (Grid array) =
    let
        i =
            x + offset (Grid array)

        j =
            y + offset (Grid array)
    in
        Grid
            <| case get j array of
                Nothing ->
                    array

                Just row ->
                    set j (set i a row) array


get2 : Position -> Grid a -> Maybe a
get2 { x, y } (Grid array) =
    case get (y + offset (Grid array)) array of
        Nothing ->
            Nothing

        Just row ->
            get (x + offset (Grid array)) row


toLists : Grid a -> List (List a)
toLists (Grid x) =
    List.map toList (toList x)


size : Grid a -> Int
size (Grid array) =
    length array
