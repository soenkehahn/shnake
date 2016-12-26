module TestUtils exposing (..)

import Test exposing (..)
import Expect exposing (..)


xdescribe : String -> List Test -> Test
xdescribe name _ =
    describe name []


isJust : (a -> Expectation) -> Maybe a -> Expectation
isJust e m =
    case m of
        Nothing ->
            fail "expected: Just _"

        Just x ->
            e x
