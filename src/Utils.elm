module Utils exposing (..)


withIndex : List a -> (( Int, a ) -> b) -> List b
withIndex list f =
    let
        inner n list =
            case list of
                [] ->
                    []

                a :: r ->
                    f ( n, a ) :: inner (n + 1) r
    in
        inner 0 list


fst : ( a, b ) -> a
fst ( a, b ) =
    a
