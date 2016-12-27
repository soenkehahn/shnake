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
            [ \() -> findLevelByStrategy <| fitnessLevel [ Left ]
            , \() -> findLevelByStrategy <| fitnessLevel [ Left, Left, Down ]
            , \() -> findLevelByStrategy <| fitnessLevel [ Down, Left ]
            , \() -> findLevelByStrategy <| fitnessLevel [ Left, Down ]
            , \() -> findLevelByStrategy <| fitnessLevel [ Left, Down, Down, Down, Right ]
            , \() -> findLevelByStrategy <| fitnessLevel [ Left, Down, Down, Right ]
            , \() -> findLevelByStrategy <| fitnessLevel [ Right, Right, Right, Down, Down, Down, Down, Right, Right ]
            ]
    in
        Maybe.map (\x -> x ())
            <| Array.get n (Array.fromList list)


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
