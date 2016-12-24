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
import Player exposing (..)
import Position exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- fixme: put shnake modules in supermodule


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
            6
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


toGo : Model -> Grid a -> Int
toGo model grid =
    (size grid ^ 2 - 1) - List.length model.player.tail


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update size msg model =
    case msg of
        Noop ->
            model ! []

        SetSeed seed ->
            { model | seed = seed } ! []

        ArrowMsg arrowMsg ->
            let
                ( newPlayer, newFood ) =
                    applyArrow arrowMsg model.food model.player
            in
                { model | player = newPlayer, food = newFood }
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


view : Int -> Model -> Html Msg
view size model =
    viewGrid model <| toGrid size model


type GridCell
    = Color String
    | NoColor


toGrid : Int -> Model -> Grid GridCell
toGrid size { player, food } =
    Grid.create size NoColor
        |> setCells food (Color "green")
        |> setCells player.tail (Color "blue")
        |> setCell player.head (Color "red")


viewGrid : Model -> Grid GridCell -> Html msg
viewGrid model grid =
    let
        a =
            30

        renderWidth =
            800

        xOffset : Int
        xOffset =
            round <| toFloat (renderWidth - a * size grid) / 2

        renderHeight =
            500

        yOffset : Int
        yOffset =
            round <| toFloat (renderHeight - a * size grid) / 2
    in
        div [ attribute "style" ("margin: auto; width: " ++ toString renderWidth ++ "px;") ]
            [ div [ attribute "style" ("width: " ++ toString renderWidth ++ "px; margin: auto; background-color: #444;") ]
                [ svg [ width (toString renderWidth), height (toString renderHeight) ]
                    (withIndex (toLists grid)
                        (\( y, row ) ->
                            g []
                                (withIndex row
                                    (\( x, cell ) ->
                                        rect
                                            [ Svg.Attributes.x (toString (x * a + xOffset))
                                            , Svg.Attributes.y (toString (y * a + yOffset))
                                            , height (toString a)
                                            , width (toString a)
                                            , fill
                                                <| case cell of
                                                    NoColor ->
                                                        "black"

                                                    Color color ->
                                                        color
                                            ]
                                            []
                                    )
                                )
                        )
                    )
                ]
            , Html.text
                (case toGo model grid of
                    0 ->
                        "You win!!!"

                    n ->
                        toString n ++ " to go..."
                )
            ]
