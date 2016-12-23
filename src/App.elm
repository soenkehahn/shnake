module App exposing (..)

import Html exposing (..)
import Keyboard exposing (..)


type alias Component msg model =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


component : Component Msg Model
component =
    { init = init
    , update = \msg model -> update msg model ! []
    , subscriptions = \_ -> subscriptions
    , view = view
    }


type alias Msg =
    Maybe Message


type Message
    = Up
    | Down
    | Left
    | Right


subscriptions : Sub Msg
subscriptions =
    let
        toArrow : KeyCode -> Maybe Message
        toArrow code =
            Debug.log (toString code)
                <| case code of
                    37 ->
                        Just Left

                    38 ->
                        Just Up

                    39 ->
                        Just Right

                    40 ->
                        Just Down

                    _ ->
                        Nothing
    in
        Sub.batch
            [ downs toArrow
            ]


type alias Model =
    { player : Position
    }


type alias Position =
    { x : Int
    , y : Int
    }


init : ( Model, Cmd Msg )
init =
    { player = Position 0 0 } ! []


update : Msg -> Model -> Model
update msg { player } =
    let
        newPosition =
            case msg of
                Just msg ->
                    case msg of
                        Up ->
                            { player | y = player.y - 1 }

                        Down ->
                            { player | y = player.y + 1 }

                        Left ->
                            { player | x = player.x - 1 }

                        Right ->
                            { player | x = player.x + 1 }

                Nothing ->
                    player
    in
        { player = newPosition }


view : Model -> Html Msg
view model =
    div []
        [ text "view"
        , pre [] [ text (toString model) ]
        ]
