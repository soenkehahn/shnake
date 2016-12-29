module Levels exposing (..)

import Level.Model exposing (..)
import Dict
import Level.Generated


all : Int -> Maybe Level
all n =
    Dict.get n
        <| Dict.fromList
        <| Level.Generated.levels
