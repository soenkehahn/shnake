module Player exposing (..)

import Position exposing (..)
import Debug exposing (..)


type alias Player =
    { head : Position
    , tail : List Position
    }


newPlayer : Player
newPlayer =
    { head = Position 0 0
    , tail = []
    }


applyArrow : ArrowMsg -> Player -> Player
applyArrow arrowMsg { head, tail } =
    let
        newHead =
            case arrowMsg of
                Up ->
                    { head | y = head.y - 1 }

                Down ->
                    { head | y = head.y + 1 }

                Left ->
                    { head | x = head.x - 1 }

                Right ->
                    { head | x = head.x + 1 }
    in
        { head = newHead, tail = moveTail head tail }


moveTail : Position -> List Position -> List Position
moveTail newHead tail =
    case tail of
        [] ->
            []

        a :: r ->
            if a == newHead then
                a :: r
            else
                newHead :: moveTail a r


addTails : Int -> Player -> Player
addTails n player =
    { player
        | tail = List.repeat n player.head ++ player.tail
    }
