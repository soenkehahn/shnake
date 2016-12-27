port module Level.Definitions exposing (..)

import Position exposing (..)
import Level.Model exposing (..)
import Level.Solution exposing (..)
import Platform exposing (program)
import Utils exposing (..)
import Dict
import Array


levels : Int -> Maybe Level
levels n =
    let
        list =
            [ \() -> findLevelByStrategy [ Left ]
            , \() -> findLevelByStrategy [ Left, Down ]
            , \() -> findLevelByStrategy [ Left, Down, Down, Down, Right ]
            , \() -> findLevelByStrategy [ Left, Down, Down, Right ]
            , \() -> findLevelByStrategy [ Down, Left ]
            , \() -> findLevelByStrategy [ Left, Left, Down ]
            , \() ->
                Level 5
                    (Position 0 0)
                    [ Position 4 4 ]
                    [ Position 0 1
                    , Position 1 1
                    , Position 2 1
                    , Position 3 3
                    , Position 4 3
                    ]
            ]
    in
        Maybe.map (\x -> x ())
            <| Array.get n (Array.fromList list)


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
