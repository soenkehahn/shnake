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


applyArrow : ArrowMsg -> List Position -> Player -> ( Player, List Position )
applyArrow arrowMsg food { head, tail } =
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

        ( newFood, eaten ) =
            List.partition (\f -> f /= newHead) food
    in
        if List.member newHead tail then
            ( { head = head, tail = tail }, food )
        else if List.length eaten > 0 then
            ( { head = newHead, tail = head :: tail }, newFood )
        else
            ( { head = newHead, tail = moveTail head tail }, food )


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


addTail : Player -> Player
addTail player =
    { player
        | tail = player.head :: player.tail
    }
