module View exposing (..)

import Config
import Css
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html.Keyed
import Layout
import Set exposing (Set)


square : List (Attribute msg) -> Html msg
square attrs =
    Layout.el
        attrs
        Layout.none


empty : Html msg
empty =
    Layout.el
        [ Html.Attributes.style "width" (String.fromInt Config.squareSize ++ "px")
        , Html.Attributes.style "height" (String.fromInt Config.squareSize ++ "px")
        ]
        Layout.none


tile :
    { nodes : Dict ( Int, Int ) Int
    , width : Int
    , height : Int
    , tiles : Dict Int { topLeft : ( Int, Int ), size : ( Int, Int ) }
    , onClick : Int -> msg
    }
    -> Html msg
tile args =
    List.range 0 (args.height - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (args.width - 1)
                    |> List.filterMap
                        (\x ->
                            Dict.get ( x, y ) args.nodes
                                |> Maybe.map
                                    (\i ->
                                        let
                                            { size, topLeft } =
                                                args.tiles
                                                    |> Dict.get i
                                                    |> Maybe.withDefault { topLeft = ( 0, 0 ), size = ( 0, 0 ) }

                                            ( width, height ) =
                                                size

                                            ( offsetX, offsetY ) =
                                                topLeft
                                        in
                                        (if i == -1 then
                                            [ Html.Attributes.style "border-radius" (String.fromInt Config.squareSize ++ "px") ]

                                         else
                                            [ ( Css.top_left
                                              , (Dict.get ( x - 1, y ) args.nodes == Just i)
                                                    || (Dict.get ( x, y - 1 ) args.nodes == Just i)
                                              )
                                            , ( Css.top_right
                                              , (Dict.get ( x + 1, y ) args.nodes == Just i)
                                                    || (Dict.get ( x, y - 1 ) args.nodes == Just i)
                                              )
                                            , ( Css.bottom_left
                                              , (Dict.get ( x - 1, y ) args.nodes == Just i)
                                                    || (Dict.get ( x, y + 1 ) args.nodes == Just i)
                                              )
                                            , ( Css.bottom_right
                                              , (Dict.get ( x + 1, y ) args.nodes == Just i)
                                                    || (Dict.get ( x, y + 1 ) args.nodes == Just i)
                                              )
                                            ]
                                                |> List.filterMap
                                                    (\( attr, bool ) ->
                                                        if bool then
                                                            Nothing

                                                        else
                                                            Just attr
                                                    )
                                        )
                                            |> (++)
                                                [ (String.fromInt (max height width * Config.squareSize) ++ "px")
                                                    |> (\string -> string ++ " " ++ string)
                                                    |> Html.Attributes.style "background-size"
                                                , (String.fromInt (-(x - offsetX) * Config.squareSize) ++ "px")
                                                    ++ " "
                                                    ++ (String.fromInt (-(y - offsetY) * Config.squareSize) ++ "px")
                                                    |> Html.Attributes.style "background-position"
                                                , Html.Attributes.style "position" "absolute"
                                                , background i |> Html.Attributes.style "background-image"
                                                , Html.Attributes.style "transition" "left ease-in-out 0.2s, top ease-in-out 0.2s"
                                                ]
                                            |> (++)
                                                (Layout.asButton
                                                    { label = "Click"
                                                    , onPress = Just (args.onClick i)
                                                    }
                                                )
                                            |> (++)
                                                (if i == -1 then
                                                    [ Html.Attributes.style "left" (String.fromInt (x * Config.squareSize + ((Config.squareSize - Config.circleSize) // 2)) ++ "px")
                                                    , Html.Attributes.style "top" (String.fromInt (y * Config.squareSize + ((Config.squareSize - Config.circleSize) // 2)) ++ "px")
                                                    , Html.Attributes.style "width" (String.fromInt Config.circleSize ++ "px")
                                                    , Html.Attributes.style "height" (String.fromInt Config.circleSize ++ "px")
                                                    ]

                                                 else
                                                    [ Html.Attributes.style "left" (String.fromInt (x * Config.squareSize) ++ "px")
                                                    , Html.Attributes.style "top" (String.fromInt (y * Config.squareSize) ++ "px")
                                                    , Html.Attributes.style "width" (String.fromInt Config.squareSize ++ "px")
                                                    , Html.Attributes.style "height" (String.fromInt Config.squareSize ++ "px")
                                                    ]
                                                )
                                            |> square
                                            |> (\html ->
                                                    ( String.fromInt i
                                                        ++ "-"
                                                        ++ String.fromInt (x - offsetX)
                                                        ++ "-"
                                                        ++ String.fromInt (y - offsetY)
                                                    , html
                                                    )
                                               )
                                    )
                        )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div" [ Html.Attributes.style "position" "relative" ]


background : Int -> String
background int =
    let
        ( c1, c2 ) =
            if int == -1 then
                ( "black", "gray" )

            else
                case int of
                    1 ->
                        ( "var(--red)", "var(--orange)" )

                    2 ->
                        ( "var(--orange)", "var(--yellow)" )

                    3 ->
                        ( "var(--yellow)", "var(--green)" )

                    4 ->
                        ( "var(--green)", "var(--cyan)" )

                    5 ->
                        ( "var(--cyan)", "var(--blue)" )

                    6 ->
                        ( "var(--blue)", "var(--violette)" )

                    7 ->
                        ( "var(--violette)", "var(--purple)" )

                    _ ->
                        ( "var(--purple)", "var(--red)" )
    in
    "linear-gradient(to bottom right," ++ c1 ++ ", " ++ c2 ++ ")"


toHtml : (Int -> msg) -> Game -> List (Html msg)
toHtml msg game =
    [ { nodes = game.board
      , width = 3
      , height = 4
      , tiles = game.tiles
      , onClick = msg
      }
        |> tile
    , Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href "style.css"
        ]
        []
    ]
