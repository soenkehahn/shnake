module Level.Model exposing (..)

import Position exposing (..)


type alias Player =
    { head : Position
    , tail : List Position
    }


newPlayer : Player
newPlayer =
    { head = Position 0 0
    , tail = []
    }


type alias Level =
    { size : Int
    , player : Position
    , food : List Position
    , walls : List Position
    }


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


init : Level -> Model
init level =
    let
        food =
            level.food
                |> List.filter (\x -> level.player /= x)
                |> List.filter (\x -> not (List.member x level.walls))

        walls =
            level.walls
                |> List.filter (\x -> level.player /= x)
    in
        Model (Player level.player []) food walls


toGo : Model -> Int
toGo model =
    List.length model.food


won =
    \model -> toGo model <= 0
