module Levels exposing (..)

import Position exposing (..)


type alias Level =
    { size : Int
    , player : Position
    , food : List Position
    }


all : List Level
all =
    [ { size = 5, player = Position 0 0, food = List.map (\x -> Position x 2) [ 0, 1, 2 ] }
    , { size = 5, player = Position 0 0, food = List.map (\x -> Position x 2) [ 0, 1, 2, 3, 4 ] }
    ]
