module App exposing (..)

import Array exposing (toList)
import Debug exposing (..)
import Grid exposing (..)
import Html.Attributes exposing (style, attribute, class)
import Html exposing (..)
import Keyboard exposing (..)
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
            ]


type alias Model =
    { player : Player
    , food : List Position
    }


newModel : Player -> Model
newModel player =
    { player = player, food = [] }


init : ( Model, Cmd Msg )
init =
    let
        model =
            newModel newPlayer
    in
        { model | food = (List.map (\x -> Position x 2) [ 0, 1, 2 ]) } ! []


toGo : Model -> Grid a -> Int
toGo model grid =
    List.length model.food


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update size msg model =
    case msg of
        Noop ->
            model ! []

        ArrowMsg arrowMsg ->
            let
                ( newPlayer, newFood ) =
                    applyArrow size arrowMsg model.food model.player
            in
                { model | player = newPlayer, food = newFood }
                    ! []


view : Int -> Model -> Html Msg
view size model =
    viewGrid model <| toGrid size model


type GridCell
    = Color String
    | NoColor


toGrid : Int -> Model -> Grid GridCell
toGrid size { player, food } =
    Grid.newGrid size NoColor
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
