module Tests exposing (..)

import Test exposing (..)
import GridTests
import PlayerTests
import RunLevelTests
import LevelSequenceTests
import LevelsTest
import UtilsTests
import StreamTests
import Level.SolutionTests


all : Test
all =
    describe "shnake tests"
        [ UtilsTests.all
        , GridTests.all
        , PlayerTests.all
        , RunLevelTests.all
        , LevelSequenceTests.all
        , LevelsTest.all
        , StreamTests.all
        , Level.SolutionTests.all False
        ]
