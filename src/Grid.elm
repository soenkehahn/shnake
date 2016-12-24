module Grid exposing (..)

import Array exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


set2 : Position -> a -> Array (Array a) -> Array (Array a)
set2 { x, y } a array =
    let
        i =
            x + 10

        j =
            y + 10
    in
        case get j array of
            Nothing ->
                array

            Just row ->
                set j (set i a row) array
