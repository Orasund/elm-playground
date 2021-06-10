module GreenFields.View.Tile exposing (view)

import Color as C
import Css exposing (backgroundColor)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GreenFields.Data.Building as Building
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.Data.Tile as Tile exposing (Tile)
import GreenFields.View.Color as Color
import Time exposing (Posix)
import Time.Extra as Time


view : { game : Game, pos : ( Int, Int ), onClick : msg, timestamp : Posix } -> Element msg
view arg =
    let
        maybeTile =
            arg.game
                |> Game.getTile arg.pos

        isOld tile =
            tile.timestamp
                |> Debug.log "tile"
                |> Time.addHours 1
                |> Time.compare (arg.timestamp |> Debug.log "timestamp")
                |> (==) GT

        onClick =
            case maybeTile of
                Just tile ->
                    if
                        isOld tile
                            && ((arg.game |> Game.getNeighbors arg.pos |> List.isEmpty |> not)
                                    || (arg.game |> Game.hasNoActives)
                               )
                    then
                        Just arg.onClick

                    else
                        Nothing

                Nothing ->
                    if arg.game |> Game.getNeighbors arg.pos |> List.isEmpty then
                        Nothing

                    else
                        Just arg.onClick

        { backgroundColor, textColor, borderColor } =
            case maybeTile of
                Just tile ->
                    if isOld tile then
                        { backgroundColor = Color.green
                        , borderColor =
                            if onClick == Nothing then
                                Color.green

                            else
                                Color.white
                        , textColor = Color.white
                        }

                    else
                        { backgroundColor = Color.white
                        , borderColor = Color.white
                        , textColor = Color.black
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
