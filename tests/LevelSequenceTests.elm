module LevelSequenceTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import LevelSequence exposing (..)
import Utils exposing (..)
import Html exposing (Html, text)


type Level
    = Level String


type alias LevelModel =
    { name : String
    , counter : Int
    , state : State
    }


type State
    = Playing
    | Won


type LMsg
    = SetState Int
    | Win


levelApi : List Level -> LevelApi Level LevelModel LMsg
levelApi levels =
    LevelApi
        { levels = levels
        , won =
            \{ state } ->
                case state of
                    Playing ->
                        False

                    Won ->
                        True
        , mkComponent =
            \(Level name) ->
                Component
                    { init = { name = name, counter = 0, state = Playing } ! []
                    , subscriptions = \_ -> Sub.none
                    , update =
                        \msg model ->
                            case msg of
                                SetState x ->
                                    { model | counter = x } ! []

                                Win ->
                                    { model | state = Won } ! []
                    , view = text << toString
                    }
        }


isLevel component expected model =
    if
        String.contains (toString (text (toString expected)))
            (toString (component.view model))
    then
        pass
    else
        fail
            (toString (component.view model)
                ++ "\n  is not (wrapped)\n"
                ++ toString (text (toString expected))
            )


all : Test
all =
    describe "LevelSequence"
        (let
            (Component component) =
                mkComponent (levelApi [ Level "a", Level "b" ])

            init =
                fst component.init
         in
            [ describe "init"
                [ test "starts the first level"
                    (\() -> isLevel component (LevelModel "a" 0 Playing) init)
                ]
            , describe "update"
                [ test "stays in the current level"
                    (\() ->
                        let
                            ( next, _ ) =
                                component.update (InnerMsg (SetState 0)) init
                        in
                            isLevel component (LevelModel "a" 0 Playing) next
                    )
                , test "relays messages"
                    (\() ->
                        let
                            ( next, _ ) =
                                component.update (InnerMsg (SetState 42)) init
                        in
                            isLevel component (LevelModel "a" 42 Playing) next
                    )
                , test "when done does not relay messages to the level anymore"
                    (\() ->
                        let
                            ( next1, _ ) =
                                component.update (InnerMsg Win) init

                            ( next2, _ ) =
                                component.update (InnerMsg (SetState 42)) next1
                        in
                            isLevel component (LevelModel "a" 0 Won) next2
                    )
                , test "when done accepts space to start the next level"
                    (\() ->
                        let
                            ( next1, _ ) =
                                component.update (InnerMsg Win) init

                            ( next2, _ ) =
                                component.update Space next1
                        in
                            isLevel component (LevelModel "b" 0 Playing) next2
                    )
                , -- fixme: space is blocked
                  test "when not done, space does nothing"
                    (\() ->
                        let
                            ( next, _ ) =
                                component.update Space init
                        in
                            isLevel component (LevelModel "a" 0 Playing) next
                    )
                ]
            ]
        )
