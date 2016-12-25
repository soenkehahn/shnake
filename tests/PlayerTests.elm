module PlayerTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import RunLevel exposing (..)
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
                                let
                                    x =
                                        newPlayer
                                in
                                    { x | tail = [ Position 0 1 ] }

                            model =
                                newModel player

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
                                Position 3 1

                            tail =
                                [ Position 3 2
                                , Position 2 2
                                , Position 1 2
                                , Position 1 3
                                ]

                            player =
                                Player head tail

                            model =
                                newModel player

                            result =
                                fst <| update 5 (ArrowMsg Left) model

                            expected =
                                { head = Position 2 1
                                , tail =
                                    [ Position 3 1
                                    , Position 3 2
                                    , Position 2 2
                                    , Position 1 2
                                    ]
                                }
                        in
                            equal expected result.player
                    )
                , test "when parts of the tail are below former snake parts"
                    (\() ->
                        let
                            tail =
                                [ Position 2 2, Position 1 2 ]

                            player =
                                let
                                    x =
                                        newPlayer
                                in
                                    { x | head = Position 2 2, tail = tail }

                            model =
                                newModel player

                            result =
                                fst <| update 5 (ArrowMsg Up) model

                            expected =
                                { head = Position 2 1
                                , tail = [ Position 2 2, Position 1 2 ]
                                }
                        in
                            equal expected result.player
                    )
                , test "not allowed to move outside of grid"
                    (\() ->
                        let
                            model =
                                let
                                    x =
                                        newPlayer
                                in
                                    newModel { x | head = Position -2 0 }

                            result =
                                fst <| update 5 (ArrowMsg Left) model
                        in
                            equal result model
                    )
                , test "not allowed to move to occupied cells"
                    (\() ->
                        let
                            tail =
                                [ Position 1 2, Position 1 1, Position 2 1 ]

                            player =
                                let
                                    x =
                                        newPlayer
                                in
                                    { x | tail = tail }

                            model =
                                newModel player

                            result =
                                fst <| update 5 (ArrowMsg Up) model
                        in
                            equal result model
                    )
                ]
            ]
        ]
