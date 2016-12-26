module Stream exposing (..)

import Debug exposing (..)


type Stream a
    = Stream (() -> ( a, Stream a ))


next : Int -> Stream a -> List a
next n (Stream s) =
    case n of
        0 ->
            []

        n ->
            let
                ( a, s2 ) =
                    s ()
            in
                a :: next (n - 1) s2


toStream : List a -> a -> Stream a
toStream list default =
    Stream
        (\() ->
            case list of
                [] ->
                    ( default, toStream [] default )

                a :: r ->
                    ( a, toStream r default )
        )


streamMap : (a -> b) -> Stream a -> Stream b
streamMap f (Stream stream) =
    Stream
        <| \() ->
            let
                ( a, next ) =
                    stream ()
            in
                ( f a, streamMap f next )


streamConcat : Stream (List a) -> Stream a
streamConcat (Stream stream) =
    let
        inner ( list, Stream next ) =
            case list of
                a :: r ->
                    ( a, Stream (\() -> inner ( r, Stream next )) )

                [] ->
                    inner (next ())
    in
        Stream (\() -> inner (stream ()))


find : (a -> Bool) -> Stream a -> a
find predicate (Stream stream) =
    let
        ( a, next ) =
            stream ()
    in
        if predicate a then
            a
        else
            find predicate next


filter : (a -> Bool) -> Stream a -> Stream a
filter predicate (Stream stream) =
    Stream
        (\() ->
            case stream () of
                ( a, next ) ->
                    if predicate a then
                        ( a, filter predicate next )
                    else
                        case filter predicate next of
                            Stream stream ->
                                stream ()
        )
