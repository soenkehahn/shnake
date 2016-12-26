module Utils exposing (..)

import Html exposing (..)


withIndex : List a -> (( Int, a ) -> b) -> List b
withIndex list f =
    let
        inner n list =
            case list of
                [] ->
                    []

                a :: r ->
                    f ( n, a ) :: inner (n + 1) r
    in
        inner 0 list


fst : ( a, b ) -> a
fst ( a, b ) =
    a


snd : ( a, b ) -> b
snd ( a, b ) =
    b


isPrefixOf : List a -> List a -> Bool
isPrefixOf a b =
    case ( b, a ) of
        ( x :: xs, y :: ys ) ->
            if x == y then
                xs |> isPrefixOf ys
            else
                False

        ( [], _ ) ->
            True

        ( _ :: _, [] ) ->
            False


unlines : List String -> String
unlines =
    String.join "\n"


type Component model msg
    = Component
        { init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        }


getComponent (Component x) =
    x
