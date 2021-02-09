module Farmig.View.Cell exposing (view, viewPlayer)

import Color exposing (Color)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Farmig.Data.Cell as Cell exposing (Cell(..))
import Farmig.Data.Item as Item exposing (Item(..))
import Farmig.View.Color as Color


viewPlayer : { food : Int, item : Maybe Item } -> Element msg
viewPlayer { food, item } =
    internal
        { text =
            if food == 0 then
                ( "💀", "" )

            else if food < 100 then
                ( "\u{1F924}", "" )

            else if food < 500 then
                ( "🙂", "" )

            else
                ( "😊", "" )
        , background =
            item
                |> Maybe.map Item.color
        , dynamicSized = False
        , onPress = Nothing
        }


view : String -> Maybe msg -> Maybe Cell -> Element msg
view tree onPress maybeCell =
    let
        text =
            maybeCell
                |> Maybe.map Cell.toString
                |> Maybe.withDefault ( tree, "" )

        color =
            case maybeCell of
                Just (Item item) ->
                    item
                        |> Item.color
                        |> Just

                _ ->
                    Nothing
    in
    internal
        { text = text
        , background = color
        , dynamicSized =
            case maybeCell of
                Just (Plant _ _) ->
                    True

                _ ->
                    False
        , onPress = onPress
        }


internal :
    { text : ( String, String )
    , background : Maybe Color
    , dynamicSized : Bool
    , onPress : Maybe msg
    }
    -> Element msg
internal { text, background, dynamicSized, onPress } =
    let
        label =
            (case text of
                ( primary, "" ) ->
                    [ primary
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Font.size
                                (if dynamicSized then
                                    40
                                        // (text
                                                |> Tuple.first
                                                |> String.length
                                           )

                                 else
                                    40
                                )
                            ]
                    ]

                ( primary, secondary ) ->
                    [ secondary
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Font.size 10
                            ]
                    , primary
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Font.size
                                (if dynamicSized then
                                    30
                                        // (text
                                                |> Tuple.first
                                                |> String.length
                                           )

                                 else
                                    30
                                )
                            ]
                    ]
            )
                |> Element.column
                    ([ Element.width <| Element.px 50
                     , Element.height <| Element.px 50
                     ]
                        ++ (background
                                |> Maybe.map
                                    (Color.toRgba
                                        >> Element.fromRgb
                                        >> Background.color
                                        >> List.singleton
                                    )
                                |> Maybe.withDefault []
                           )
                    )
    in
    if onPress == Nothing then
        label

    else
        Input.button
            [ Element.mouseDown []
            , Element.focused []
            , Element.mouseOver []
            ]
            { label = label
            , onPress = onPress
            }
