module Farmig.Data.Input exposing (Input, decoder)

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


decoder : (Maybe ( Int, Int ) -> msg) -> Decoder msg
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
                |> Maybe.map fromDirection
                |> msgMapper
        )
        (D.field "key" D.string)
