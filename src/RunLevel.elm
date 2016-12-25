module RunLevel exposing (..)

import Array exposing (toList)
import Debug exposing (..)
import Grid exposing (..)
import Html.Attributes exposing (style, attribute, class)
import Html exposing (..)
import Levels exposing (Level)
import Keyboard exposing (..)
import Utils exposing (..)
import Player exposing (..)
import Position exposing (..)
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import LevelSequence exposing (LevelApi(..))


-- fixme: put shnake modules in supermodule


levelApi : LevelApi Level Model Msg
levelApi =
    LevelApi
        { levels = Levels.all
        , mkComponent = component
        , won = \model -> toGo model <= 0
        }


component : Level -> Component Model Msg
component level =
    let
        size =
            level.size
    in
        Component
            { init = init level
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
        downs toArrow


type alias Model =
    { player : Player
    , food : List Position
    , walls : List Position
    }


newModel : Player -> Model
newModel player =
    { player = player
    , food = []
    , walls = []
    }


setWalls : List Position -> Model -> Model
setWalls walls model =
    { model | walls = walls }


init : Level -> ( Model, Cmd Msg )
init level =
    let
        food =
            level.food
                |> List.filter (\x -> level.player /= x)
                |> List.filter (\x -> not (List.member x level.walls))
    in
        Model (Player level.player []) food level.walls ! []


toGo : Model -> Int
toGo model =
    List.length model.food


isDone : Model -> Bool
isDone model =
    List.length model.food <= 0


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update size msg model =
    case msg of
        Noop ->
            model ! []

        ArrowMsg arrowMsg ->
            let
                ( newPlayer, newFood ) =
                    applyArrow size arrowMsg model.food model.walls model.player
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
toGrid size { player, food, walls } =
    Grid.newGrid size NoColor
        |> setCells food (Color "green")
        |> setCells walls (Color "#444")
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
            600

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
                (case toGo model of
                    0 ->
                        "You win!!!"

                    n ->
                        toString n ++ " to go..."
                )
            ]
