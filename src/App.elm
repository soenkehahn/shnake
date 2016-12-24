module App exposing (..)

import Array exposing (toList)
import Debug exposing (..)
import Grid exposing (..)
import Html.Attributes exposing (style, attribute, class)
import Html exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
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
    let
        size =
            21
    in
        { init = init
        , update = update size
        , subscriptions = \_ -> subscriptions
        , view = view size
        }


type Msg
    = Noop
    | ArrowMsg ArrowMsg
    | Tick
    | NewFood Seed


type ArrowMsg
    = Up
    | Down
    | Left
    | Right


subscriptions : Sub Msg
subscriptions =
    let
        toArrow : KeyCode -> Msg
        toArrow code =
            case code of
                37 ->
                    ArrowMsg Left

                38 ->
                    ArrowMsg Up

                39 ->
                    ArrowMsg Right

                40 ->
                    ArrowMsg Down

                _ ->
                    Noop
    in
        Sub.batch
            [ downs toArrow
            , every second (\_ -> Tick)
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


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update size msg model =
    (\( a, b ) -> ( normalize a, b ))
        <| case msg of
            Noop ->
                model ! []

            ArrowMsg arrowMsg ->
                { model
                    | player = updatePlayerPosition arrowMsg model.player
                }
                    ! []

            Tick ->
                model ! [ Random.generate (NewFood << initialSeed) (int minInt maxInt) ]

            NewFood seed ->
                { model | food = randomPosition seed size model.player :: model.food } ! []


normalize : Model -> Model
normalize model =
    let
        newFood =
            List.filter (\f -> f /= model.player) model.food
    in
        { model | food = newFood }


view : Int -> Model -> Html Msg
view size model =
    viewGrid <| toGrid size model


type GridCell
    = Color String
    | NoColor


toGrid : Int -> Model -> Grid GridCell
toGrid size { player, food } =
    Grid.create size NoColor
        |> set2 player (Color "red")
        |> (\grid -> List.foldl (\foodItem -> set2 foodItem (Color "green")) grid food)


viewGrid : Grid GridCell -> Html msg
viewGrid grid =
    div []
        <| for (toLists grid)
        <| \row ->
            div [ attribute "style" "display: block; height: 12px;" ]
                (for row
                    <| \cell ->
                        div (cellStyle cell)
                            []
                )


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
