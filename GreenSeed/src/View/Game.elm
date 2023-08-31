module View.Game exposing (..)

import Cell
import Config
import Dict
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Layout
import View.Cell


toHtml : List (Attribute msg) -> { selectPos : ( Int, Int ) -> msg } -> Game -> Html msg
toHtml attrs args game =
    [ "Money: "
        ++ String.fromInt game.money
        |> Layout.text []
    , asField [] { selectPos = args.selectPos } game
    ]
        |> Layout.column attrs


asField : List (Attribute msg) -> { selectPos : ( Int, Int ) -> msg } -> Game -> Html msg
asField attrs args game =
    List.range 0 (Config.fieldSize - 1)
        |> List.map
            (\y ->
                List.range 0 (Config.fieldSize - 1)
                    |> List.map
                        (\x ->
                            game.field
                                |> Dict.get ( x, y )
                                |> Maybe.withDefault Cell.soil
                                |> View.Cell.toHtml
                                    (Layout.asButton
                                        { label = "Select " ++ String.fromInt x ++ ", " ++ String.fromInt y
                                        , onPress = args.selectPos ( x, y ) |> Just
                                        }
                                    )
                        )
                    |> Layout.row []
            )
        |> Layout.column attrs
