module Level.GenerationTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Level.Generation exposing (..)
import Stream exposing (..)
import Random exposing (..)


all : Test
all =
    describe "Level.Generation"
        [ describe "randomLevels"
            [ test "generates a stream of random levels"
                (\() ->
                    let
                        levels =
                            next 3 (randomLevels (initialSeed 124325425))
                    in
                        levels
                            |> equal
                                [ { size = 11
                                  , player = { x = 10, y = 2 }
                                  , food =
                                        [ { x = 8, y = 8 }
                                        , { x = 2, y = 0 }
                                        , { x = 6, y = 1 }
                                        , { x = 9, y = 9 }
                                        , { x = 0, y = 8 }
                                        , { x = 10, y = 2 }
                                        , { x = 3, y = 2 }
                                        , { x = 2, y = 10 }
                                        , { x = 8, y = 7 }
                                        , { x = 3, y = 4 }
                                        , { x = 7, y = 8 }
                                        , { x = 2, y = 1 }
                                        , { x = 9, y = 3 }
                                        , { x = 1, y = 8 }
                                        ]
                                  , walls = [ { x = 8, y = 10 } ]
                                  }
                                , { size = 8
                                  , player = { x = 4, y = 4 }
                                  , food = [ { x = 6, y = 3 }, { x = 7, y = 7 } ]
                                  , walls = []
                                  }
                                , { size = 3
                                  , player = { x = 1, y = 1 }
                                  , food =
                                        [ { x = 2, y = 1 }
                                        , { x = 0, y = 1 }
                                        , { x = 0, y = 1 }
                                        , { x = 1, y = 0 }
                                        ]
                                  , walls = []
                                  }
                                ]
                )
            ]
        ]
