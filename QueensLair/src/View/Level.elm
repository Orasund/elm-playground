module View.Level exposing (..)

import Config
import Dict
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import Piece exposing (Piece)
import Square


viewSquare : List (Attribute msg) -> { label : String, onPress : Maybe msg } -> Maybe String -> Html msg
viewSquare attrs args string =
    string
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


toHtml : { selected : Maybe ( Int, Int ), onSelect : Maybe ( Int, Int ) -> msg, movementOverride : Maybe Piece } -> Level -> Html msg
toHtml args game =
    let
        isValidMove move =
            args.movementOverride
                |> Maybe.map
                    (\piece ->
                        { game
                            | board =
                                game.board
                                    |> Dict.insert move.from
                                        { isWhite = True
                                        , piece = piece
                                        }
                        }
                    )
                |> Maybe.withDefault game
                |> Level.isValidMove move
    in
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
                                                if isValidMove { from = from, to = ( x, y ) } then
                                                    "green"

                                                else if x + y |> modBy 2 |> (==) 0 then
                                                    "white"

                                                else
                                                    "grey"
                                           )
                            in
                            (case game.board |> Dict.get ( x, y ) of
                                Just square ->
                                    Square.toString square |> Just

                                Nothing ->
                                    if game.loot == Just ( x, y ) then
                                        Just "💰"

                                    else
                                        Nothing
                            )
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

                                                else if isValidMove { from = from, to = ( x, y ) } then
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
