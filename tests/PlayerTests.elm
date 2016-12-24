module PlayerTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import App exposing (..)
import Position exposing (..)
import Utils exposing (..)
import Player exposing (..)


all : Test
all =
    describe "Player"
        [ describe "move"
            [ describe "tail gets dragged around by the head"
                [ test "simple case"
                    (\() ->
                        let
                            player =
                                { newPlayer | tail = [ Position -1 0 ] }

                            model =
                                Model player []

                            result =
                                fst <| update 5 (ArrowMsg Right) model

                            expected =
                                { head = Position 1 0
                                , tail = [ Position 0 0 ]
                                }
                        in
                            equal expected result.player
                    )
                , test "more complicated case"
                    (\() ->
                        let
                            head =
                                Position 1 -1

                            tail =
                                [ Position 1 0
                                , Position 0 0
                                , Position -1 0
                                , Position -1 1
                                ]

                            player =
                                Player head tail

                            model =
                                Model player []

                            result =
                                fst <| update 5 (ArrowMsg Left) model

                            expected =
                                { head = Position 0 -1
                                , tail =
                                    [ Position 1 -1
                                    , Position 1 0
                                    , Position 0 0
                                    , Position -1 0
                                    ]
                                }
                        in
                            equal expected result.player
                    )
                , test "when parts of the tail are below former snake parts"
                    (\() ->
                        let
                            tail =
                                [ Position 0 0, Position -1 0 ]

                            player =
                                { newPlayer | tail = tail }

                            model =
                                Model player []

                            result =
                                fst <| update 5 (ArrowMsg Up) model

                            expected =
                                { head = Position 0 -1
                                , tail = [ Position 0 0, Position -1 0 ]
                                }
                        in
                            equal expected result.player
                    )
                , test "not allowed to move to occupied cells"
                    (\() ->
                        let
                            tail =
                                [ Position -1 0, Position -1 -1, Position 0 -1 ]

                            player =
                                { newPlayer | tail = tail }

                            model =
                                Model player []

                            result =
                                fst <| update 5 (ArrowMsg Up) model
                        in
                            equal result model
                    )
                ]
            ]
        ]
