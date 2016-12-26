module StreamTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Stream exposing (..)
import Debug exposing (..)


all : Test
all =
    describe "Stream"
        [ describe "filter"
            [ test "filters even numbers"
                (\() ->
                    let
                        nats =
                            let
                                inner n =
                                    Stream (\() -> ( n, inner (n + 1) ))
                            in
                                inner 0

                        even : Int -> Bool
                        even n =
                            n % 2 == 0

                        filtered =
                            next 5
                                <| Stream.filter even nats
                    in
                        filtered |> equal [ 0, 2, 4, 6, 8 ]
                )
            ]
        ]
