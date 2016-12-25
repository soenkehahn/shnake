module Tests exposing (..)

import Test exposing (..)
import GridTests
import PlayerTests
import RunLevelTests
import LevelSequenceTests


all : Test
all =
    describe "shnake tests"
        [ GridTests.all
        , PlayerTests.all
        , RunLevelTests.all
        , LevelSequenceTests.all
        ]
