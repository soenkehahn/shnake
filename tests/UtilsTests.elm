module UtilsTests exposing (..)

import Utils exposing (..)
import Test exposing (..)
import Expect exposing (..)


all : Test
all =
    describe "Utils"
        [ describe "isPrefixOf"
            [ test "it detects a prefix"
                (\() -> equal True ([ 1, 2, 3 ] |> isPrefixOf [ 1, 2, 3, 4, 5 ]))
            , test "returns true for the same inputs"
                (\() -> equal True ([ 1, 2, 3 ] |> isPrefixOf [ 1, 2, 3 ]))
            , test "detects a non-prefix"
                (\() -> equal False ([ 4, 2, 3 ] |> isPrefixOf [ 1, 2, 3, 4, 5 ]))
            , test "returns false if prefix is too long"
                (\() -> equal False ([ 1, 2, 3 ] |> isPrefixOf [ 1, 2 ]))
            ]
        , describe "deleteByIndex"
            [ test "works"
                (\() ->
                    deleteByIndex 1 [ 1, 2, 3 ] |> equal [ 1, 3 ]
                )
            ]
        , describe "addByIndex"
            [ test "allows to add in the middle"
                (\() -> addByIndex 2 42 [ 1, 2, 3, 4 ] |> equal [ 1, 2, 42, 3, 4 ])
            , test "allows to add at the beginning"
                (\() -> addByIndex 0 42 [ 1, 2, 3 ] |> equal [ 42, 1, 2, 3 ])
            , test "allows to add at the end"
                (\() -> addByIndex 3 42 [ 1, 2, 3 ] |> equal [ 1, 2, 3, 42 ])
            ]
        ]
