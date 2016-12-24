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
    | SetSeed Seed
    | NewFood


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
            , every second (\_ -> NewFood)
            ]


type alias Model =
    { player : Player
    , food : List Position
    , seed : Seed
    }


newModel : Player -> Model
newModel player =
    { player = player, food = [], seed = initialSeed 0 }


init : ( Model, Cmd Msg )
init =
    newModel newPlayer
        ! [ Random.generate (SetSeed << initialSeed) (int minInt maxInt)
          ]


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update size msg model =
    (\( a, b ) -> ( normalize a, b ))
        <| case msg of
            Noop ->
                model ! []

            SetSeed seed ->
                { model | seed = seed } ! []

            ArrowMsg arrowMsg ->
                { model
                    | player = applyArrow arrowMsg model.player
                }
                    ! []

            NewFood ->
                let
                    ( newFood, seed2 ) =
                        randomPosition model.seed size model.player.head
                in
                    { model
                        | food = newFood :: model.food
                        , seed = seed2
                    }
                        ! []


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
        |> setCells food (Color "green")
        |> set player.head (Color "red")
        |> setCells player.tail (Color "red")


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
