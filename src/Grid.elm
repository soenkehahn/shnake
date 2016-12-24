module Grid exposing (..)

import Array exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


get2 : Int -> Int -> Array (Array a) -> Maybe a
get2 x y array =
    case get y array of
        Nothing ->
            Nothing

        Just row ->
            get x row


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
