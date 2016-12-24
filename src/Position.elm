module Position exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type ArrowMsg
    = Up
    | Down
    | Left
    | Right


