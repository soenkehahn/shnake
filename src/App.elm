module App exposing (..)

-- fixme: combine tick and newfood
-- fixme: put seed in model?

import Array exposing (toList)
import Debug exposing (..)
import Grid exposing (..)
import Html.Attributes exposing (style, attribute, class)
import Html exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import Time exposing (..)
import Utils exposing (..)
import Player exposing (..)
import Position exposing (..)


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
    { player : Player
    , food : List Position
    }


init : ( Model, Cmd Msg )
init =
    { player = newPlayer, food = [] } ! []


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update size msg model =
    (\( a, b ) -> ( normalize a, b ))
        <| case msg of
            Noop ->
                model ! []

            ArrowMsg arrowMsg ->
                { model
                    | player = applyArrow arrowMsg model.player
                }
                    ! []

            Tick ->
                model ! [ Random.generate (NewFood << initialSeed) (int minInt maxInt) ]

            NewFood seed ->
                { model | food = randomPosition seed size model.player.head :: model.food } ! []


normalize : Model -> Model
normalize model =
    let
        ( newFood, eaten ) =
            List.partition (\f -> f /= model.player.head) model.food
    in
        { model
            | food = newFood
            , player = addTails (List.length eaten) model.player
        }


view : Int -> Model -> Html Msg
view size model =
    viewGrid <| toGrid size model


type GridCell
    = Color String
    | NoColor


toGrid : Int -> Model -> Grid GridCell
toGrid size { player, food } =
    Grid.create size NoColor
        -- fixme: add setCells
        |>
            (\grid -> List.foldl (\foodItem -> set foodItem (Color "green")) grid food)
        |>
            set player.head (Color "red")
        |>
            (\grid ->
                List.foldl (\tailItem -> set tailItem (Color "red"))
                    grid
                    player.tail
            )


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
