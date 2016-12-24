module Tests exposing (..)

import Test exposing (..)
import GridTests
import PlayerTests
import AppTests


all : Test
all =
    describe "shnake tests"
        [ GridTests.all
        , PlayerTests.all
        , AppTests.all
        ]
