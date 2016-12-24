module Utils exposing (..)


for : List a -> (a -> b) -> List b
for list f =
    List.map f list


fst : ( a, b ) -> a
fst ( a, b ) =
    a
