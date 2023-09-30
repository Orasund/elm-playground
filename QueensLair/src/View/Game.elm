module View.Game exposing (..)

import Config
import Dict
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Square exposing (Square)


viewSquare : List (Attribute msg) -> { label : String, onPress : Maybe msg } -> Maybe Square -> Html msg
viewSquare attrs args square =
    square
        |> Maybe.map Square.toString
        |> Maybe.withDefault ""
        |> Layout.text
            ([ Html.Attributes.style "width" "40px"
             , Html.Attributes.style "height" "40px"
             , Html.Attributes.style "font-size" "40px"
             ]
                ++ Layout.asButton args
                ++ Layout.centered
                ++ attrs
            )


toHtml : { selected : Maybe ( Int, Int ), onSelect : Maybe ( Int, Int ) -> msg } -> Game -> Html msg
toHtml args game =
    List.range 0 (Config.boardSize - 1)
        |> List.map
            (\y ->
                List.range 0 (Config.boardSize - 1)
                    |> List.map
                        (\x ->
                            let
                                color =
                                    args.selected
                                        |> Maybe.withDefault ( -1, -1 )
                                        |> (\from ->
                                                if Game.isValidMove { from = from, to = ( x, y ) } game then
                                                    "green"

                                                else if x + y |> modBy 2 |> (==) 0 then
                                                    "white"

                                                else
                                                    "grey"
                                           )
                            in
                            game.board
                                |> Dict.get ( x, y )
                                |> viewSquare
                                    [ (if args.selected == Just ( x, y ) then
                                        "green"

                                       else
                                        color
                                      )
                                        |> (\c -> Html.Attributes.style "border" ("5px dotted " ++ c))
                                    , Html.Attributes.style "background-color" color
                                    ]
                                    { label = "Select " ++ String.fromInt x ++ "," ++ String.fromInt y
                                    , onPress =
                                        case args.selected of
                                            Just from ->
                                                if ( x, y ) == from then
                                                    args.onSelect Nothing |> Just

                                                else if Game.isValidMove { from = from, to = ( x, y ) } game then
                                                    args.onSelect (Just ( x, y )) |> Just

                                                else
                                                    Nothing

                                            Nothing ->
                                                if
                                                    game.board
                                                        |> Dict.get ( x, y )
                                                        |> Maybe.map .isWhite
                                                        |> Maybe.withDefault False
                                                then
                                                    args.onSelect (Just ( x, y )) |> Just

                                                else
                                                    Nothing
                                    }
                        )
                    |> Layout.row []
            )
        |> Layout.column []
