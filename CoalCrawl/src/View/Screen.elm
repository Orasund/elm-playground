module View.Screen exposing (..)

import Config
import Data.Block exposing (Block)
import Data.Game exposing (Game)
import Data.Tile
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Tile


fromGame : { onPress : ( Int, Int ) -> msg, camera : ( Int, Int ) } -> Game -> Html msg
fromGame args game =
    List.range 0 (Config.height - 1)
        |> List.map
            (\y ->
                List.range 0 (Config.width - 1)
                    |> List.map
                        (\x ->
                            let
                                ( playerX, playerY ) =
                                    args.camera

                                pos =
                                    ( playerX + x - Config.width // 2
                                    , playerY + y - Config.height // 2
                                    )
                            in
                            if pos == game.player.pos then
                                Data.Tile.fromPlayer game.player
                                    |> View.Tile.toHtml

                            else
                                game.world
                                    |> Dict.get pos
                                    |> Maybe.map Data.Tile.fromBlock
                                    |> Maybe.withDefault Data.Tile.wall
                                    |> View.Tile.toHtml
                                    |> Layout.el
                                        ((if game.selected == pos then
                                            [ Attr.style "background-color" "yellow" ]

                                          else
                                            []
                                         )
                                            ++ Layout.asButton
                                                { onPress = pos |> args.onPress |> Just
                                                , label = "Activate"
                                                }
                                        )
                        )
                    |> Layout.row []
            )
        |> Layout.column []
