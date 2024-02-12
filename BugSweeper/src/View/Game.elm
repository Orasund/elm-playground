module View.Game exposing (..)

import BugSpecies
import Config
import Dict
import Game exposing (Game, Tile(..))
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Object
import View.Square


tile : List (Attribute msg) -> ( Int, Int ) -> Game -> Html msg
tile attrs ( x, y ) game =
    let
        revealed =
            Dict.member ( x, y ) game.revealed

        currentTile =
            game.tiles |> Dict.get ( x, y )

        string =
            if revealed then
                case currentTile of
                    Just (ObjectTile object) ->
                        Object.toString object

                    Just (BugTile bug) ->
                        BugSpecies.toString bug

                    Nothing ->
                        "❌"

            else
                ""
    in
    string
        |> (if revealed then
                case currentTile of
                    Just (BugTile _) ->
                        View.Square.revealedAndCaptured attrs

                    _ ->
                        View.Square.revealed attrs

            else
                View.Square.default attrs
           )


board : { onSelect : ( Int, Int ) -> msg } -> Game -> Html msg
board args game =
    List.range 0 (Config.gridSize - 1)
        |> List.map
            (\y ->
                List.range 0 (Config.gridSize - 1)
                    |> List.map
                        (\x ->
                            tile
                                (if Dict.member ( x, y ) game.revealed then
                                    []

                                 else
                                    Layout.asButton
                                        { label = "Reveal " ++ String.fromInt x ++ "," ++ String.fromInt y
                                        , onPress = args.onSelect ( x, y ) |> Just
                                        }
                                )
                                ( x, y )
                                game
                        )
                    |> Layout.row [ Layout.noWrap, Layout.gap 8 ]
            )
        |> Layout.column
            (Layout.centered
                ++ [ Layout.gap 8
                   , Html.Attributes.class "emoji-color-font"
                   ]
            )