module View.Screen exposing (..)

import Config
import Data.Game exposing (Game)
import Data.Tile
import Data.Zoom exposing (Zoom)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Color
import View.Tile


animation : { width : Int, height : Int } -> Game -> Html msg
animation args game =
    List.range 0 (args.height - 1)
        |> List.map
            (\y ->
                List.range 0 (args.width - 1)
                    |> List.map
                        (\x ->
                            tile { onPress = Nothing, zoom = Data.Zoom.none } ( x, y ) game
                        )
                    |> Layout.row [ Layout.noWrap ]
            )
        |> Layout.column []


fromGame : { onPress : ( Int, Int ) -> msg, camera : ( Int, Int ), zoom : Zoom } -> Game -> Html msg
fromGame args game =
    List.range 0 (Config.height args.zoom - 1)
        |> List.map
            (\y ->
                List.range 0 (Config.width args.zoom - 1)
                    |> List.map
                        (\x ->
                            let
                                ( playerX, playerY ) =
                                    args.camera

                                pos =
                                    ( playerX + x - Config.width args.zoom // 2
                                    , playerY + y - Config.height args.zoom // 2
                                    )
                            in
                            tile { onPress = args.onPress pos |> Just, zoom = args.zoom } pos game
                        )
                    |> Layout.row [ Layout.noWrap ]
            )
        |> Layout.column []


tile : { onPress : Maybe msg, zoom : Zoom } -> ( Int, Int ) -> Game -> Html msg
tile args pos game =
    (if pos == game.player.pos then
        Data.Tile.fromPlayer game game.player

     else
        Data.Tile.fromPos pos game
    )
        |> (\list ->
                (if List.isEmpty list then
                    [ Data.Tile.wall ]

                 else
                    list
                )
                    |> List.map (View.Tile.toHtml args.zoom)
                    |> List.map (\html -> ( [], html ))
                    |> Layout.stack
                        ((if game.selected == pos then
                            [ Attr.style "background-color" View.Color.yellow ]

                          else if list == [] then
                            [ Attr.style "background-color" View.Color.black ]

                          else
                            []
                         )
                            ++ (args.onPress
                                    |> Maybe.map
                                        (\_ ->
                                            Layout.asButton
                                                { onPress = args.onPress
                                                , label = "Activate"
                                                }
                                        )
                                    |> Maybe.withDefault []
                               )
                            ++ [ Attr.style "width" (Config.tileSize args.zoom)
                               , Attr.style "height" (Config.tileSize args.zoom)
                               ]
                        )
           )
