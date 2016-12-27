port module Level.Definitions exposing (..)

import Position exposing (..)
import Level.Model exposing (..)
import Level.Solution exposing (..)
import Platform exposing (program)
import Utils exposing (..)
import Dict
import Array
import LocalSearch exposing (..)
import Debug exposing (..)


levels : Int -> Maybe Level
levels n =
    let
        list =
            [ fitnessLevel [ Left ]
            , fitnessLevel [ Left, Left, Down ]
            , fitnessLevel [ Down, Left ]
            , fitnessLevel [ Left, Down ]
            , fitnessLevel [ Left, Down, Down, Down, Right ]
            , fitnessLevel [ Left, Down, Down, Right ]
            , fitnessLevel [ Right, Right, Right, Down, Down, Down, Down, Right, Right ]
            , len 0
            , len 1
            , len 2
            , len 3
            , len 4
            , len 5
            , len 6
            , len 7
            , len2 1
            , len2 2
            , len2 3
            , len2 4
            , len2 5
            ]
    in
        Maybe.map (\f -> findLevelByStrategy <| f)
            <| Array.get n (Array.fromList list)


len : Int -> Fitness Level
len n level =
    case findShortestSolution n level of
        Nothing ->
            10000000

        Just shortest ->
            let
                lengthPenalty =
                    abs (n - List.length shortest) * 1000

                diversityPenalty =
                    case firstGroupLength shortest of
                        Nothing ->
                            0

                        Just n ->
                            abs (List.length shortest - n) * 1
            in
                lengthPenalty + diversityPenalty


len2 : Int -> Fitness Level
len2 n level =
    case findShortestSolution n level of
        Nothing ->
            10000000

        Just shortest ->
            let
                lengthPenalty =
                    abs (n - List.length shortest) * 1000

                diversityPenalty =
                    case firstGroupLength shortest of
                        Nothing ->
                            0

                        Just n ->
                            countEqualNeighbors shortest * 1
            in
                lengthPenalty + diversityPenalty


countEqualNeighbors : List a -> Int
countEqualNeighbors list =
    case list of
        [] ->
            0

        [ x ] ->
            0

        a :: b :: r ->
            (if a == b then
                1
             else
                0
            )
                + countEqualNeighbors (b :: r)



-- + diversityPenalty


fitnessLevel : List Direction -> Fitness Level
fitnessLevel strategy level =
    if not <| isSolution level strategy then
        invalidSolutionWeight
    else
        case findShortestSolution (List.length strategy - 1) level of
            Nothing ->
                0

            Just shorter ->
                List.length strategy - List.length shorter


invalidSolutionWeight : Int
invalidSolutionWeight =
    20000


shorterStrategyWeight : Int
shorterStrategyWeight =
    1



-- * code generation


main =
    program
        { init = Dict.empty ! []
        , subscriptions = \_ -> generateLevel (\x -> x)
        , update =
            \n dict ->
                case levels n of
                    Nothing ->
                        dict ! []

                    Just level ->
                        let
                            new =
                                Dict.insert n level dict
                        in
                            new
                                ! [ writeCode (generateCode (Dict.toList new))
                                  ]
        }


port writeCode : String -> Cmd msg


port generateLevel : (Int -> msg) -> Sub msg


generateCode : List ( Int, Level ) -> String
generateCode levels =
    unlines
        ([ "module Level.Generated exposing (levels)"
         , ""
         , "import Level.Model exposing (..)"
         , ""
         , "levels : List (Int, Level)"
         , "levels ="
         ]
            ++ (List.map (\l -> "  (" ++ toString l ++ ") ::") levels)
            ++ [ "  []"
               , ""
               ]
        )
