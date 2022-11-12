module View.Screen exposing (..)

import Config
import Data.Game exposing (Game)
import Data.Tile
import Data.World
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
                            tile Nothing ( x, y ) game
                        )
                    |> Layout.row [ Layout.noWrap ]
            )
        |> Layout.column []


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
                            tile (args.onPress pos |> Just) pos game
                        )
                    |> Layout.row [ Layout.noWrap ]
            )
        |> Layout.column []


tile : Maybe msg -> ( Int, Int ) -> Game -> Html msg
tile maybeOnPress pos game =
    (if pos == game.player.pos then
        Data.Tile.fromPlayer game.player
            |> Just

     else
        game.world
            |> Data.World.get pos
            |> Maybe.map (Data.Tile.fromBlock game)
    )
        |> (\maybe ->
                maybe
                    |> Maybe.withDefault Data.Tile.wall
                    |> View.Tile.toHtml
                    |> Layout.el
                        ((if game.selected == pos then
                            [ Attr.style "background-color" View.Color.yellow ]

                          else if maybe == Nothing then
                            [ Attr.style "background-color" View.Color.black ]

                          else
                            []
                         )
                            ++ (maybeOnPress
                                    |> Maybe.map
                                        (\_ ->
                                            Layout.asButton
                                                { onPress = maybeOnPress
                                                , label = "Activate"
                                                }
                                        )
                                    |> Maybe.withDefault []
                               )
                        )
           )
