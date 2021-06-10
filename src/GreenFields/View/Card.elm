module GreenFields.View.Card exposing (view)

import Color as C
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GreenFields.Data.Building as Building
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.View.Color as Color


view : { selected : Maybe ( Int, Int ), onRestore : msg, game : Game } -> Element msg
view arg =
    (arg.selected
        |> Maybe.andThen
            (\pos ->
                arg.game
                    |> Game.getTile pos
                    |> Maybe.map
                        (\tile ->
                            [ tile.building
                                |> Building.getName
                                |> Element.text
                            , "==========" |> Element.text
                            , " " |> Element.text
                            , Input.button
                                [ Color.green
                                    |> C.toRgba
                                    |> Element.fromRgb
                                    |> Font.color
                                , Font.bold
                                ]
                                { onPress = Just arg.onRestore
                                , label = "[Build]" |> Element.text
                                }
                            ]
                                |> Element.column []
                        )
            )
        |> Maybe.withDefault
            ("Click on a white square to view the details"
                |> Element.text
                |> List.singleton
                |> Element.paragraph [ Element.centerY ]
            )
    )
        |> Element.el
            ([ Element.alignLeft
             , Element.padding 16
             , Element.width <| Element.px 200
             , Element.height <| Element.px 300
             , Element.centerY
             , Border.rounded 16
             ]
                ++ (case arg.selected of
                        Just pos ->
                            [ Color.white
                                |> C.toRgba
                                |> Element.fromRgb
                                |> Background.color
                            , Color.black
                                |> C.toRgba
                                |> Element.fromRgb
                                |> Font.color
                            ]

                        Nothing ->
                            [ Color.white
                                |> C.toRgba
                                |> Element.fromRgb
                                |> Font.color
                            ]
                   )
            )
