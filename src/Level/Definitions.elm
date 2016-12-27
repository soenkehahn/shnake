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
            [ {- \() -> findLevelByStrategy <| fitnessLevel [ Left ]
                 , \() -> findLevelByStrategy <| fitnessLevel [ Left, Left, Down ]
                 ,
              -}
              {- \() -> findLevelByStrategy <| len 0
                 , \() -> findLevelByStrategy <| len 1
                 , \() -> findLevelByStrategy <| len 2
                 , \() -> findLevelByStrategy <| len 3
                 , \() -> findLevelByStrategy <| len 4
                 , \() -> findLevelByStrategy <| len 5
                 , \() -> findLevelByStrategy <| len 6
                 , \() -> findLevelByStrategy <| len 7
              -}
              len2 1
            , len2 2
            , len2 3
            , len2 4
            , len2 5
--            , len2 6
              -- , \() -> findLevelByStrategy <| fitnessLevel [ Down, Left ]
              -- , \() -> findLevelByStrategy <| fitnessLevel [ Left, Down ]
              -- , \() -> findLevelByStrategy <| fitnessLevel [ Left, Down, Down, Down, Right ]
              -- , \() -> findLevelByStrategy <| fitnessLevel [ Left, Down, Down, Right ]
              -- , \() -> findLevelByStrategy <| fitnessLevel [ Right, Right, Right, Down, Down, Down, Down, Right, Right ]
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
