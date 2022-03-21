module Ruz.View.Board exposing (view)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed
import Layout
import Ruz.Config as Config
import Ruz.Data.Figure as Figure exposing (Figure, FigureId)
import Ruz.Data.Overlay exposing (Overlay(..))
import Ruz.View.Figure as Figure


view :
    { figures : Dict FigureId Figure
    , overlay : Dict ( Int, Int ) Overlay
    , onClick : ( Int, Int ) -> msg
    , gameOver : Bool
    }
    -> Dict FigureId ( Int, Int )
    -> Html msg
view args figurePos =
    let
        gridSize =
            Config.boardSize / toFloat Config.size
    in
    [ [ args.figures
            |> viewFigures { figurePos = figurePos }
      , figurePos
            |> viewBoard
                { figures = args.figures
                , overlay = args.overlay
                , onClick = args.onClick
                }
      ]
    , if args.gameOver then
        [ Html.text "Game Over"
            |> Layout.el
                [ Attr.style "background-color" "rgba(0,0,0,0.5)"
                , Attr.style "height" (String.fromFloat Config.boardSize ++ "px")
                , Attr.style "color" "white"
                , Attr.style "z-index" "2"
                , Attr.style "font-size" "30px"
                , Layout.alignCenter
                , Layout.centerContent
                , Attr.style "backdrop-filter" "blur(2px)"
                ]
        ]

      else
        []
    ]
        |> List.concat
        |> Html.div
            [ Attr.style "height" (String.fromFloat Config.boardSize ++ "px")
            , Attr.style "width" (String.fromFloat Config.boardSize ++ "px")
            , Attr.style "position" "relative"
            , Attr.style "border" ("solid 1px " ++ Color.toCssString Color.black)
            ]


viewFigures : { figurePos : Dict FigureId ( Int, Int ) } -> Dict FigureId Figure -> Html msg
viewFigures args figures =
    let
        gridSize =
            100 / toFloat Config.size
    in
    args.figurePos
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map
            (\( figureId, ( x, y ) ) ->
                ( "figure-" ++ String.fromInt figureId
                , [ figures
                        |> Dict.get figureId
                        |> Maybe.map (Figure.toString False)
                        |> Maybe.withDefault (Figure.toString True Figure.player)
                        |> Figure.view [ Attr.style "width" "100%" ]
                  ]
                    |> Html.div
                        [ Attr.style "position" "absolute"
                        , Attr.style "left" (String.fromFloat (toFloat x * gridSize + gridSize / 4) ++ "%")
                        , Attr.style "top" (String.fromFloat (toFloat y * gridSize + gridSize / 4) ++ "%")
                        , Attr.style "width" (String.fromFloat (gridSize / 2) ++ "%")
                        , Attr.style "height" (String.fromFloat (gridSize / 2) ++ "%")
                        , Attr.style "display" "flex"
                        , Attr.style "justify-content" "center"
                        , Attr.style "align-items" "center"
                        , Attr.style "transition" "top 0.5s, left 0.5s"
                        , Attr.style "font-size" "40px"
                        ]
                )
            )
        |> Html.Keyed.node "div"
            [ Attr.style "position" "absolute"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            ]


viewBoard :
    { figures : Dict Int Figure
    , overlay : Dict ( Int, Int ) Overlay
    , onClick : ( Int, Int ) -> msg
    }
    -> Dict FigureId ( Int, Int )
    -> Html msg
viewBoard args figurePos =
    List.repeat Config.size ()
        |> List.indexedMap
            (\j () ->
                List.repeat Config.size ()
                    |> List.indexedMap
                        (\i () ->
                            let
                                cell attrs =
                                    Html.div
                                        ([ Attr.style "border"
                                            ("solid 1px "
                                                ++ (Config.gray |> Color.toCssString)
                                            )
                                         , Layout.fill
                                         , Attr.style "height" "calc(100% -  2px)"
                                         ]
                                            ++ attrs
                                        )
                                        []

                                clickableCell attrs =
                                    Html.a
                                        ([ Event.onClick (args.onClick ( i, j ))
                                         , Attr.href "#"
                                         , Attr.style "border" ("solid 1px " ++ (Config.gray |> Color.toCssString))
                                         , Layout.fill
                                         , Attr.style "height" "calc(100% -  2px)"
                                         ]
                                            ++ attrs
                                        )
                                        []
                            in
                            case args.overlay |> Dict.get ( i, j ) of
                                Just Success ->
                                    [ Attr.style "background-color" (Color.toCssString Config.green) ]
                                        |> clickableCell

                                Just Warning ->
                                    [ Attr.style "background-color" (Color.toCssString Config.green)
                                    , Attr.style "background"
                                        ("repeating-linear-gradient("
                                            ++ " -45deg,"
                                            ++ (Color.toCssString Config.green ++ ",")
                                            ++ (Color.toCssString Config.green ++ " 20%,")
                                            ++ ((Config.gray |> Color.toCssString) ++ " 20%,")
                                            ++ ((Config.gray |> Color.toCssString) ++ " 40%")
                                            ++ ")"
                                        )
                                    ]
                                        |> clickableCell

                                Just Danger ->
                                    [ Attr.style "background-color" (Config.gray |> Color.toCssString) ]
                                        |> clickableCell

                                Nothing ->
                                    cell []
                        )
                    |> Layout.row
                        [ Attr.style "flex" "1"
                        ]
            )
        |> Layout.column
            [ Attr.style "height" "100%"
            , Attr.style "width" "100%"
            , Attr.style "position" "absolute"
            ]
