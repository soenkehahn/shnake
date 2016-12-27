module LocalSearch exposing (..)

import Random exposing (..)
import Debug exposing (..)
import Utils exposing (..)


type alias Mutate a =
    a -> Generator a


type alias Fitness a =
    a -> Int


search : Mutate a -> Fitness a -> a -> a
search mutate fitness current =
    let
        inner : a -> Generator a
        inner current =
            if fitness current == 0 then
                succeed current
            else
                mutate current
                    |> andThen
                        (\mutated ->
                            if fitness mutated <= fitness current then
                                inner mutated
                            else
                                inner current
                        )
    in
        fst <| step (inner current) (initialSeed 39430549853406587)


succeed : a -> Generator a
succeed a =
    map (\_ -> a) bool
