module Level.Solution exposing (..)

import Position exposing (..)
import Random exposing (..)
import Debug exposing (..)
import Stream exposing (..)
import Utils exposing (..)
import Player exposing (..)
import Level.Model exposing (..)
import Level.Generation exposing (..)


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
                directions
    in
        if won result then
            Wins
        else
            Looses


findLevelByStrategy : List Direction -> Level
findLevelByStrategy strategy =
    let
        maxLength =
            List.length strategy
    in
        find
            (\level ->
                Just strategy == log "shortest" (findShortestSolution maxLength level)
            )
            (randomLevels (initialSeed 223423423))


findShortestSolution : Int -> Level -> Maybe (List Direction)
findShortestSolution maxLength level =
    let
        init =
            Level.Model.init level

        inner : Stream (List Direction) -> Maybe (List Direction)
        inner (Stream stream) =
            let
                ( strategy, next ) =
                    log "inner"
                        <| stream ()
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
