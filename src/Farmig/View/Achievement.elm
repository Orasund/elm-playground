module Farmig.View.Achievement exposing (view)

import Element
import Element.Font as Font
import Farmig.Data.Achievement as Achievement exposing (Achievement)
import Farmig.Data.Cell as Cell exposing (Cell(..))
import Farmig.Data.Food exposing (Food(..))
import Farmig.Data.Item exposing (Item(..))
import Widget
import Widget.Material as Material


view : Achievement -> Widget.Item msg
view achievement =
    Widget.multiLineItem (Material.multiLineItem Material.defaultPalette)
        { title = Achievement.get achievement |> .title
        , text =
            achievement
                |> Achievement.next
                |> Maybe.map (Achievement.get >> .challenge)
                |> Maybe.withDefault "Want to play again?"
        , onPress = Nothing
        , icon =
            \{ size } ->
                Achievement.get achievement
                    |> .icon
                    |> Cell.toString
                    |> Tuple.first
                    |> Element.text
                    |> Element.el
                        [ Element.width <| Element.px size
                        , Element.height <| Element.px size
                        , Font.family
                            [ Font.external
                                { url = "font.css"
                                , name = "Noto Emoji"
                                }
                            ]
                        ]
        , content = always Element.none
        }
