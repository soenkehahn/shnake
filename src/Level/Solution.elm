module Level.Solution exposing (..)

import Position exposing (..)
import Debug exposing (..)
import Stream exposing (..)
import Utils exposing (..)
import Player exposing (..)
import Level.Model exposing (..)


type RunResult
    = Wins
    | Looses


simulatePlayer : Level -> List Direction -> RunResult
simulatePlayer level directions =
    let
        init =
            Level.Model.init level

        result =
            List.foldl (applyArrow level.size)
                init
                (log "directions" directions)
    in
        if won result then
            Wins
        else
            Looses


findShortestSolution : Level -> List Direction
findShortestSolution level =
    let
        init =
            Level.Model.init level

        inner : Stream (List Direction) -> List Direction
        inner (Stream stream) =
            let
                ( strategy, next ) =
                    stream ()
            in
                case simulate level.size ( [], [] ) strategy init of
                    Repeats looping ->
                        inner (removeByPrefix looping next)

                    Failure ->
                        inner next

                    Success solution ->
                        solution
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
