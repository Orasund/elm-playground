module Bomb16.Data.Input exposing (Direction(..), Input, decoder, fromDirection)

import Json.Decode as D exposing (Decoder)


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Input =
    ( Int, Int )


fromDirection : Direction -> ( Int, Int )
fromDirection dir =
    case dir of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


decoder : (Maybe Direction -> msg) -> Decoder msg
decoder msgMapper =
    D.map
        (\string ->
            (case string of
                "ArrowLeft" ->
                    Just Left

                "a" ->
                    Just Left

                "A" ->
                    Just Left

                "ArrowRight" ->
                    Just Right

                "d" ->
                    Just Right

                "D" ->
                    Just Right

                "ArrowUp" ->
                    Just Up

                "w" ->
                    Just Up

                "W" ->
                    Just Up

                "ArrowDown" ->
                    Just Down

                "s" ->
                    Just Down

                "S" ->
                    Just Down

                _ ->
                    Nothing
            )
                |> msgMapper
        )
        (D.field "key" D.string)
