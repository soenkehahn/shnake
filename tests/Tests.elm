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
import Level.GenerationTests
import LocalSearchTests
import Level.DefinitionsTests
import Level.ModelTests


runSlowTests : Bool
runSlowTests =
    False


all : Test
all =
    describe "shnake tests"
        [ UtilsTests.all
        , GridTests.all
        , PlayerTests.all
        , RunLevelTests.all
        , LevelSequenceTests.all
        , LevelsTest.all runSlowTests
        , StreamTests.all
        , Level.SolutionTests.all runSlowTests
        , Level.GenerationTests.all
        , LocalSearchTests.all
        , Level.DefinitionsTests.all
        , Level.ModelTests.all
        ]
