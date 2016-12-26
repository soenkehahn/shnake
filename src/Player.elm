module Player exposing (..)

import Position exposing (..)
import Debug exposing (..)
import Grid exposing (..)
import Level.Model exposing (..)


applyArrow :
    Int
    -> Direction
    -> Model
    -> Model
applyArrow size arrowMsg model =
    let
        { player, food, walls } =
            model

        { head, tail } =
            player

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
        if not (inGrid size newHead) then
            model
        else if List.member newHead tail then
            model
        else if List.member newHead walls then
            model
        else if List.length eaten > 0 then
            { model
                | player =
                    { head = newHead
                    , tail = head :: tail
                    }
                , food = newFood
            }
        else
            { model
                | player =
                    { head = newHead
                    , tail = moveTail head tail
                    }
                , food = food
            }


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
