module LevelSequence exposing (..)

import Html exposing (..)
import Utils exposing (..)
import Keyboard exposing (..)
import Levels exposing (..)
import Debug exposing (..)


type LevelApi level lModel lMsg
    = LevelApi
        { levels : List level
        , won : lModel -> Bool
        , mkComponent : level -> Component lModel lMsg
        }


mkComponent : LevelApi level lModel lMsg -> Component (Model level lModel lMsg) (Msg lMsg)
mkComponent (LevelApi api) =
    case api.levels of
        [] ->
            crash "no levels"

        a :: r ->
            Component
                { init = init (LevelApi api) a r
                , subscriptions = subscriptions
                , update = update (LevelApi api)
                , view = view
                }


type Model level lModel lMsg
    = Model
        { levels : List level
        , currentComponent : Component lModel lMsg
        , currentModel : lModel
        }
    | Done


type Msg lMsg
    = Noop
    | InnerMsg lMsg
    | Next


init :
    LevelApi level lModel lMsg
    -> level
    -> List level
    -> ( Model level lModel lMsg, Cmd (Msg lMsg) )
init (LevelApi api) a r =
    let
        (Component component) =
            api.mkComponent a

        ( model, cmd ) =
            component.init
    in
        ( Model
            { levels = r
            , currentComponent = Component component
            , currentModel = model
            }
        , Cmd.map InnerMsg cmd
        )


subscriptions : Model level lModel lMsg -> Sub (Msg lMsg)
subscriptions model =
    case model of
        Done ->
            Sub.none

        Model model ->
            let
                (Component component) =
                    model.currentComponent
            in
                Sub.batch
                    [ Sub.map InnerMsg
                        (component.subscriptions model.currentModel)
                    , downs
                        (\key ->
                            if List.member key [ 13, 32 ] then
                                Next
                            else
                                Noop
                        )
                    ]


update :
    LevelApi level lModel lMsg
    -> Msg lMsg
    -> Model level lModel lMsg
    -> ( Model level lModel lMsg, Cmd (Msg lMsg) )
update (LevelApi api) msg model =
    case model of
        Done ->
            Done ! []

        Model model ->
            let
                (Component component) =
                    model.currentComponent
            in
                case msg of
                    Noop ->
                        Model model ! []

                    InnerMsg msg ->
                        if not <| api.won model.currentModel then
                            let
                                ( new, cmd ) =
                                    component.update msg model.currentModel
                            in
                                ( Model { model | currentModel = new }, Cmd.map InnerMsg cmd )
                        else
                            Model model ! []

                    Next ->
                        if not <| api.won model.currentModel then
                            Model model ! []
                        else
                            case model.levels of
                                [] ->
                                    Done ! []

                                a :: r ->
                                    let
                                        (Component component) =
                                            api.mkComponent a

                                        ( init, cmd ) =
                                            component.init
                                    in
                                        Model
                                            { levels = r
                                            , currentComponent = Component component
                                            , currentModel = init
                                            }
                                            ! [ Cmd.map InnerMsg cmd ]


view model =
    case model of
        Model { currentComponent, currentModel } ->
            Html.map InnerMsg
                <| (getComponent currentComponent).view currentModel

        Done ->
            text "No more levels... :("
