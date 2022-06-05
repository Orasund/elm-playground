module View.Grid exposing (..)

import Config
import Css
import Dict exposing (Dict)
import Game exposing (Tile(..))
import Gen.Enum.Natural exposing (Natural(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Insect exposing (Insect(..))
import View.Natural as Natural


tileWidth : Int
tileWidth =
    80


viewTile : Tile -> Html msg
viewTile tile =
    (case tile of
        NaturalTile natural ->
            natural |> Natural.view

        InsectTile insect ->
            case insect of
                Caterpillar ->
                    "🐛"

                Beetle ->
                    "\u{1FAB2}"

                Snail ->
                    "🐌"
    )
        |> Html.text


view : (Int -> msg) -> Dict ( Int, Int ) Tile -> Html msg
view onClick dict =
    List.range 0 (Config.columns - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.rows - 1)
                    |> List.map
                        (\y ->
                            dict
                                |> Dict.get ( x, y )
                                |> Maybe.map
                                    (\tile ->
                                        [ viewTile tile
                                        ]
                                    )
                                |> Maybe.withDefault []
                                |> Html.a
                                    [ Attr.css
                                        [ tileWidth |> toFloat |> Css.px |> Css.height
                                        , tileWidth |> toFloat |> Css.px |> Css.width
                                        , Css.absolute |> Css.position
                                        , y * tileWidth |> toFloat |> Css.px |> Css.top
                                        , x * tileWidth |> toFloat |> Css.px |> Css.left
                                        , Css.border2 (Css.px 1) Css.solid
                                        , toFloat tileWidth / 2 |> Css.px |> Css.fontSize
                                        , Css.textDecoration Css.none
                                        , Css.alignItems Css.center
                                        , Css.displayFlex
                                        , Css.justifyContent Css.center
                                        ]
                                    , Attr.href "#"
                                    , Events.onClick (onClick x)
                                    ]
                        )
            )
        |> Html.div
            [ Attr.css
                [ Config.columns * tileWidth |> toFloat |> Css.px |> Css.width
                , Config.rows * tileWidth |> toFloat |> Css.px |> Css.height
                ]
            ]
