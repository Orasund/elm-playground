module View.Game exposing (..)

import Bug
import Collection exposing (Variant(..))
import Config
import Dict
import Game exposing (Game, Tile(..))
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
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
                        Bug.toString bug

                    Nothing ->
                        "❌"

            else
                ""
    in
    string
        |> (if revealed then
                case currentTile of
                    Just (BugTile _) ->
                        case Dict.get ( x, y ) game.revealed of
                            Just Royal ->
                                View.Square.revealedAndSpecialCaptured attrs

                            _ ->
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
                    |> Html.div [ Html.Style.gapPx 8 ]
            )
        |> Html.div
            [ Html.Style.flexDirectionColumn
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 8
            , Html.Attributes.class "emoji-color-font"
            ]
