port module Level.Definitions exposing (..)

import Position exposing (..)
import Level.Model exposing (..)
import Level.Solution exposing (..)
import Platform exposing (program)
import Utils exposing (..)
import Dict exposing (Dict)
import Array
import LocalSearch exposing (..)
import RunLevel exposing (..)


-- import Debug exposing (..)


levels : Int -> Maybe Level
levels n =
    let
        list =
            [ judge
            ]
    in
        Maybe.map (\f -> findLevel 10000 <| f)
            <| Array.get n (Array.fromList list)


judge : Fitness ( Level, List Direction )
judge ( levelInput, strategy ) =
    let
        level =
            normalize levelInput

        initial =
            init level

        redundantMovesPenalty : List Direction -> Model -> Maybe Int
        redundantMovesPenalty list model =
            if won model then
                Just <| List.length list
            else
                case list of
                    [] ->
                        Just 10000

                    a :: r ->
                        let
                            new =
                                fst <| update level.size (ArrowMsg a) model
                        in
                            if model == new then
                                Just 10000000
                            else
                                redundantMovesPenalty r new

        lengthPenalty =
            abs (List.length strategy - 100)

        foodPenalty =
            abs (List.length level.food - 10)
    in
        [ case simulatePlayer level strategy of
            Wins ->
                0

            Looses ->
                1
        , lengthPenalty
        , case redundantMovesPenalty strategy initial of
            Nothing ->
                0

            Just x ->
                x
        , foodPenalty
        ]



-- * code generation


main : Program Never (Dict Int Level) Int
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
