module Level.Solution exposing (..)

import Position exposing (..)
import Random exposing (..)
import Debug exposing (..)
import Stream exposing (..)
import Utils exposing (..)
import Player exposing (..)
import Level.Model exposing (..)
import LocalSearch exposing (..)


type RunResult
    = Wins
    | Looses


isSolution : Level -> List Direction -> Bool
isSolution level strategy =
    simulatePlayer level strategy == Wins


simulatePlayer : Level -> List Direction -> RunResult
simulatePlayer level directions =
    let
        init =
            Level.Model.init level

        result =
            List.foldl (applyArrow level.size)
                init
                directions
    in
        if won result then
            Wins
        else
            Looses


findLevel : Int -> Fitness ( Level, List Direction ) -> Level
findLevel searchLimit fitness =
    let
        mutate =
            mutateTuple mutateLevel (mutateList directionGen mutateDirection)
    in
        fst <| search searchLimit mutate fitness ( Level 9 (Position 0 0) [] [], [] )


mutateLevel : Mutate Level
mutateLevel level =
    int 1 3
        |> andThen
            (\which ->
                case which of
                    1 ->
                        mutatePosition level.size level.player
                            |> map (\new -> { level | player = new })

                    2 ->
                        mutateList (randomPosition level.size)
                            (mutatePosition level.size)
                            level.food
                            |> map (\new -> { level | food = new })

                    3 ->
                        mutateList (randomPosition level.size)
                            (mutatePosition level.size)
                            level.walls
                            |> map (\new -> { level | walls = new })

                    n ->
                        crash ("mutateLevel: shouldn't happen: " ++ toString n)
            )


mutatePosition : Int -> Mutate Position
mutatePosition size position =
    let
        clampToGrid : Int -> Int
        clampToGrid =
            clamp 0 (size - 1)
    in
        map2
            (\xDiff yDiff ->
                { x = clampToGrid (position.x + xDiff), y = clampToGrid (position.y + yDiff) }
            )
            (int -3 3)
            (int -3 3)


randomPosition : Int -> Generator Position
randomPosition size =
    map2 (\x y -> Position x y) (int 0 size) (int 0 size)


findShortestSolution : Int -> Level -> Maybe (List Direction)
findShortestSolution maxLength level =
    let
        init =
            Level.Model.init level

        inner : Stream (List Direction) -> Maybe (List Direction)
        inner (Stream stream) =
            let
                ( strategy, next ) =
                    stream ()
            in
                if List.length strategy > maxLength then
                    Nothing
                else
                    case simulate level.size ( [], [] ) strategy init of
                        Repeats looping ->
                            inner (removeByPrefix looping next)

                        Failure ->
                            inner next

                        Success solution ->
                            Just solution
    in
        inner allStrategies


removeByPrefix : List a -> Stream (List a) -> Stream (List a)
removeByPrefix prefix stream =
    Stream.filter (\list -> not (prefix |> isPrefixOf list)) stream


allStrategies : Stream (List Direction)
allStrategies =
    let
        result =
            Stream (\() -> ( [], streamConcat (streamMap prefixDirs result) ))

        prefixDirs : List Direction -> List (List Direction)
        prefixDirs l =
            [ Up :: l
            , Down :: l
            , Left :: l
            , Right :: l
            ]
    in
        result


type SimResult
    = Repeats (List Direction)
    | Success (List Direction)
    | Failure


simulate : Int -> ( List Model, List Direction ) -> List Direction -> Model -> SimResult
simulate size history strategy model =
    if won model then
        Success (List.reverse (snd history))
    else if List.member model (fst history) then
        Repeats (List.reverse (snd history))
    else
        case strategy of
            [] ->
                Failure

            direction :: restStrategy ->
                let
                    nextModel =
                        applyArrow size direction model

                    newHistory =
                        ( model :: fst history, direction :: snd history )
                in
                    simulate size newHistory restStrategy nextModel
