module GreenFields.View.Tile exposing (view)

import Color as C
import Css exposing (backgroundColor)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GreenFields.Data.Building as Building
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.Data.Tile as Tile
import GreenFields.View.Color as Color
import Time exposing (Posix)


view : { game : Game, pos : ( Int, Int ), onClick : msg, timestamp : Posix } -> Element msg
view arg =
    let
        maybeTile =
            arg.game
                |> Game.getTile arg.pos

        onClick =
            case maybeTile of
                Just tile ->
                    if
                        (Tile.isOld arg.timestamp tile
                            && ((arg.game
                                    |> Game.getNeighbors arg.pos
                                    |> List.filter
                                        (\( _, pos ) ->
                                            arg.game |> Game.isActive pos
                                        )
                                    |> List.isEmpty
                                    |> not
                                )
                                    || (arg.game |> Game.hasNoActives)
                               )
                        )
                            || ((Tile.isOld arg.timestamp tile |> not)
                                    && (arg.game |> Game.isActive arg.pos)
                               )
                    then
                        Just arg.onClick

                    else
                        Nothing

                Nothing ->
                    if
                        arg.game
                            |> Game.getNeighbors arg.pos
                            |> List.filter
                                (\( _, pos ) ->
                                    arg.game |> Game.isActive pos
                                )
                            |> List.isEmpty
                    then
                        Nothing

                    else
                        Just arg.onClick

        { backgroundColor, textColor, borderColor } =
            case maybeTile of
                Just tile ->
                    if tile |> Tile.isOld arg.timestamp then
                        { backgroundColor = Color.green
                        , borderColor =
                            if onClick == Nothing then
                                Color.green

                            else
                                Building.color tile.building
                        , textColor = Building.color tile.building
                        }

                    else if arg.game |> Game.isActive arg.pos then
                        { backgroundColor = Building.color tile.building
                        , borderColor = Color.white
                        , textColor = Color.black
                        }

                    else
                        { backgroundColor = Color.black
                        , borderColor = Color.black
                        , textColor = Building.color tile.building
                        }

                Nothing ->
                    { backgroundColor = Color.green
                    , borderColor =
                        if onClick == Nothing then
                            Color.green

                        else
                            Color.white
                    , textColor = Color.white
                    }

        text =
            case maybeTile of
                Just tile ->
                    tile.building |> Building.getSymbol

                Nothing ->
                    if onClick == Nothing then
                        ""

                    else
                        "+"

        size =
            32

        content =
            text
                |> Element.text
                |> Element.el [ Element.centerX, Element.centerY ]

        attr =
            [ backgroundColor
                |> C.toRgba
                |> Element.fromRgb
                |> Background.color
            , textColor
                |> C.toRgba
                |> Element.fromRgb
                |> Font.color
            , borderColor
                |> C.toRgba
                |> Element.fromRgb
                |> Border.color
            , Border.rounded (size // 4)
            , Border.width 1
            , Element.width <| Element.px size
            , Element.height <| Element.px size
            ]
    in
    if onClick == Nothing then
        content
            |> Element.el attr

    else
        Input.button attr
            { onPress = onClick
            , label = content
            }
