module Main exposing (..)

import Html exposing (..)
import App


main : Program Never App.Model App.Msg
main =
    program App.component
