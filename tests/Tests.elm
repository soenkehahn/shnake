module Tests exposing (..)

import Test exposing (..)
import Test.Html.Query exposing (..)
import Test.Html.Selector exposing (..)
import Expect exposing (..)
import String
import App exposing (..)


all : Test
all =
    describe "Sample Test Suite"
        [ describe "update"
            [ describe "arrow messages"
                (let
                    testArrowMsg msg newPosition =
                        test (toString msg)
                            <| \() ->
                                let
                                    init =
                                        Model (Position 0 0)

                                    expected =
                                        Model newPosition
                                in
                                    equal expected (update (Just msg) init)
                 in
                    [ testArrowMsg Up (Position 0 (0 - 1))
                    , testArrowMsg Down (Position 0 1)
                    , testArrowMsg Left (Position (0 - 1) 0)
                    , testArrowMsg Right (Position 1 0)
                    ]
                )
            ]
        , describe "view"
            (let
                model =
                    Model (Position 0 0)
             in
                [ test "has cells"
                    <| \() ->
                        view model
                            |> fromHtml
                            |> findAll [ class "cell" ]
                            |> each (has [])
                ]
            )
        ]
