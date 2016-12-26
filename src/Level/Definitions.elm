port module Level.Definitions exposing (..)

import Position exposing (..)
import Level.Model exposing (..)
import Level.Solution exposing (..)
import Platform exposing (program)
import Utils exposing (..)
import Dict


levels : Int -> Maybe Level
levels n =
    case n of
        0 ->
            Just <| findLevelByStrategy [ Left ]

        1 ->
            Just <| findLevelByStrategy [ Left, Left, Down ]

        2 ->
            Just
                <| Level 5
                    (Position 0 0)
                    [ Position 4 4 ]
                    [ Position 0 1
                    , Position 1 1
                    , Position 2 1
                    , Position 3 3
                    , Position 4 3
                    ]

        _ ->
            Nothing


main =
    program
        { init = Dict.empty ! []
        , subscriptions = \_ -> generateLevel (\x -> x)
        , update =
            \n dict ->
                let
                    newDict =
                        case levels n of
                            Nothing ->
                                dict

                            Just level ->
                                Dict.insert n level dict
                in
                    newDict
                        ! [ writeCode (generateCode (Dict.toList newDict))
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
