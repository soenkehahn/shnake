module LocalSearch exposing (..)

import Random exposing (..)
import Debug exposing (..)
import Utils exposing (..)
import Array


type alias Mutate a =
    a -> Generator a


type alias Fitness a =
    a -> Int


search : Int -> Mutate a -> Fitness a -> a -> a
search limit mutate fitness current =
    let
        inner : Int -> a -> Generator a
        inner n current =
            if n >= limit || fitness current <= 0 then
                succeed
                    <| log ("fitness: " ++ toString (fitness current))
                        current
            else
                mutate current
                    |> andThen
                        (\mutated ->
                            if fitness mutated <= fitness current then
                                inner (n + 1) mutated
                            else
                                inner (n + 1) current
                        )
    in
        fst <| step (inner 0 current) (initialSeed 39430549853406587)


succeed : a -> Generator a
succeed a =
    map (\_ -> a) bool


mutateList : Generator a -> Mutate a -> Mutate (List a)
mutateList generateNew mutateElement list =
    case list of
        [] ->
            generateNew |> map (\x -> [ x ])

        _ :: _ ->
            int 1 3
                |> andThen
                    (\which ->
                        case which of
                            1 ->
                                -- mutate element
                                let
                                    array =
                                        Array.fromList list
                                in
                                    int 0 (Array.length array - 1)
                                        |> andThen
                                            (\index ->
                                                case Array.get index array of
                                                    Nothing ->
                                                        crash "mutateList: shouldn't happen"

                                                    Just element ->
                                                        mutateElement element
                                                            |> map
                                                                (\new ->
                                                                    Array.toList (Array.set index new array)
                                                                )
                                            )

                            2 ->
                                -- add element
                                map2 (\index new -> addByIndex index new list)
                                    (int 0 (List.length list - 1))
                                    generateNew

                            3 ->
                                -- delete element
                                int 0 (List.length list - 1)
                                    |> map
                                        (\index ->
                                            deleteByIndex index list
                                        )

                            n ->
                                crash ("mutateList: shouldn't happen: " ++ toString n)
                    )
