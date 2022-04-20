module DungeonSokoban.View.Game exposing (..)

import Array
import DungeonSokoban.Data.Cell exposing (Cell(..))
import DungeonSokoban.Data.Game as Game exposing (Game)
import Grid
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import Svg


viewSquare : Html msg -> Html msg
viewSquare =
    Layout.el
        [ Attr.style "width" "100px"
        , Attr.style "height" "100px"
        , Layout.centerContent
        , Layout.alignCenter
        ]


viewCell : Maybe Cell -> Html msg
viewCell maybeCell =
    viewSquare
        (case maybeCell of
            Nothing ->
                Layout.none

            Just (Monster _) ->
                Layout.el
                    [ Attr.style "width" "80%"
                    , Attr.style "height" "80%"
                    , Attr.style "background-color" "red"
                    , Attr.style "border-radius" "100px"
                    ]
                    Layout.none

            Just Box ->
                Layout.el
                    [ Attr.style "width" "40%"
                    , Attr.style "height" "40%"
                    , Attr.style "border-color" "darkGray"
                    , Attr.style "border-width" "20px"
                    , Attr.style "border-radius" "20px"
                    , Attr.style "border-style" "solid"
                    ]
                    Layout.none

            Just Hole ->
                Layout.el
                    [ Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "background-color" "black"
                    ]
                    Layout.none
        )


viewPlayer : Html msg
viewPlayer =
    viewSquare
        (Layout.el
            [ Attr.style "width" "40%"
            , Attr.style "height" "40%"
            , Attr.style "border-radius" "100px"
            , Attr.style "border-color" "lightBlue"
            , Attr.style "border-width" "20px"
            , Attr.style "border-style" "solid"
            ]
            Layout.none
        )


view : Game -> Html msg
view game =
    game.board
        |> Array.toList
        |> List.indexedMap
            (\j row ->
                row
                    |> Array.toList
                    |> List.indexedMap
                        (\i maybeCell ->
                            if ( i, j ) == game.player then
                                viewPlayer

                            else
                                viewCell maybeCell
                        )
                    |> Layout.row []
            )
        |> Layout.column [ Attr.style "height" "400px", Attr.style "width" "400px" ]
