module Grid exposing (..)

import Array exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type Grid a
    = Grid (Array (Array a))


toLists : Grid a -> List (List a)
toLists (Grid x) =
    List.map toList (toList x)


create : Int -> a -> Grid a
create size a =
    Grid (Array.repeat size (Array.repeat size a))


set2 : Position -> a -> Grid a -> Grid a
set2 { x, y } a (Grid array) =
    let
        offset =
            floor (toFloat (length array) / 2)

        i =
            x + offset

        j =
            y + offset
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
