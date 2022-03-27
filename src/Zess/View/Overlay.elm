module Zess.View.Overlay exposing (createCellOverlay, viewGameOver)

import Dict exposing (Dict)
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Layout
import Zess.Config as Config
import Zess.Data.Game as Game exposing (Change, Game)
import Zess.Data.Overlay exposing (Overlay(..))


viewGameOver : msg -> Int -> Html msg
viewGameOver onClick score =
    let
        ( green, alpha, hint ) =
            if score < 50 then
                ( 0, 0.5, "Keep Trying" )

            else if score < 100 then
                ( 100, 0.7, "Nice" )

            else if score < 200 then
                ( 150, 0.7, "Very Good" )

            else
                ( 200, 0.7, "Legendary!" )
    in
    [ Html.text "Game Over" |> Layout.el [ Attr.style "font-size" "30px" ]
    , Html.text hint
    ]
        |> Layout.column
            [ Attr.style "background-color" ("rgba(0," ++ String.fromInt green ++ ",0," ++ String.fromFloat alpha ++ ")")
            , Attr.style "height" (String.fromFloat Config.boardSize ++ "px")
            , Attr.style "color" "white"
            , Attr.style "z-index" "2"
            , Layout.spacing 40
            , Layout.alignCenter
            , Layout.centerContent
            , Attr.style "backdrop-filter" "blur(2px)"
            ]
        |> List.singleton
        |> Html.a
            [ Attr.href "#"
            , Attr.style "text-decoration" "none"
            , Events.onClick onClick
            ]


createCellOverlay : { game : Game, changes : List Change, gameOver : Bool } -> Dict ( Int, Int ) Overlay
createCellOverlay ({ game } as model) =
    if model.changes /= [] then
        Dict.empty

    else
        List.range 0 (Config.size - 1)
            |> List.concatMap
                (\i ->
                    List.range 0 (Config.size - 1)
                        |> List.map (\j -> ( i, j ))
                )
            |> List.filterMap
                (\pos ->
                    if model.gameOver then
                        Nothing

                    else if pos == model.game.player then
                        Nothing

                    else if model.gameOver then
                        Nothing

                    else if Game.valid { isEnemy = False, from = model.game.player, to = pos } model.game then
                        if
                            model.game.grid
                                |> Dict.keys
                                |> List.any
                                    (\enemyPos ->
                                        (enemyPos /= model.game.player)
                                            && (enemyPos /= pos)
                                            && Game.valid { isEnemy = True, from = enemyPos, to = pos }
                                                { game
                                                    | grid =
                                                        game.grid
                                                            |> Dict.remove model.game.player
                                                            |> Dict.remove pos
                                                    , player = pos
                                                }
                                    )
                        then
                            Just ( pos, Danger )

                        else if model.game |> Game.isDangerous pos then
                            Just ( pos, Warning )

                        else
                            Just ( pos, Success )

                    else
                        Nothing
                )
            |> Dict.fromList
