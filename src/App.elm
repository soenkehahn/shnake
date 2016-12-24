module App exposing (..)

import Array exposing (..)
import Grid exposing (..)
import Html.Attributes exposing (style, attribute, class)
import Html exposing (..)
import Keyboard exposing (..)
import Random
import Time exposing (..)
import Utils exposing (..)


type alias Component msg model =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


component : Component Msg Model
component =
    { init = init
    , update = update
    , subscriptions = \_ -> subscriptions
    , view = view
    }


type alias Msg =
    Maybe Message


type Message
    = ArrowMsg ArrowMsg
    | Tick
    | NewFood Position


type ArrowMsg
    = Up
    | Down
    | Left
    | Right


subscriptions : Sub Msg
subscriptions =
    let
        toArrow : KeyCode -> Maybe Message
        toArrow code =
            case code of
                37 ->
                    Just <| ArrowMsg Left

                38 ->
                    Just <| ArrowMsg Up

                39 ->
                    Just <| ArrowMsg Right

                40 ->
                    Just <| ArrowMsg Down

                _ ->
                    Nothing
    in
        Sub.batch
            [ downs toArrow
            , every second (\_ -> Just Tick)
            ]


type alias Model =
    { player : Position
    , food : List Position
    }


init : ( Model, Cmd Msg )
init =
    { player = Position 0 0, food = [] } ! []


updatePlayerPosition : ArrowMsg -> Position -> Position
updatePlayerPosition arrowMsg player =
    case arrowMsg of
        Up ->
            { player | y = player.y - 1 }

        Down ->
            { player | y = player.y + 1 }

        Left ->
            { player | x = player.x - 1 }

        Right ->
            { player | x = player.x + 1 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            model ! []

        Just msg ->
            case msg of
                ArrowMsg arrowMsg ->
                    { model
                        | player = updatePlayerPosition arrowMsg model.player
                    }
                        ! []

                Tick ->
                    let
                        x =
                            Random.int -10 10

                        y =
                            Random.int -10 10

                        pos =
                            Random.map2 Position x y
                    in
                        model ! [ Random.generate (Just << NewFood) pos ]

                NewFood pos ->
                    { model | food = pos :: model.food } ! []


view : Model -> Html Msg
view model =
    viewGrid <| toGrid model


viewGrid : Grid GridCell -> Html msg
viewGrid (Grid grid) =
    div []
        [ div []
            (for (toList grid)
                <| \row ->
                    div [ attribute "style" "display: block; height: 12px;" ]
                        (for (toList row)
                            <| \cell ->
                                div (cellStyle cell)
                                    []
                        )
            )
        ]


type GridCell
    = Color String
    | NoColor


toGrid : Model -> Grid GridCell
toGrid { player, food } =
    Array.repeat 21 (Array.repeat 21 NoColor)
        |> Grid
        |> set2 player (Color "red")


cellStyle : GridCell -> List (Attribute msg)
cellStyle gridCell =
    [ class "cell"
    , attribute "style"
        ("height: 10px; width: 10px; border: 1px solid; display: inline-block;"
            ++ case gridCell of
                NoColor ->
                    ""

                Color color ->
                    "background-color: " ++ color ++ ";"
        )
    ]
