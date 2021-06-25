module Bomb16.View.Cell exposing (view, viewPlayer)

import Bomb16.Data.Cell as Cell exposing (Cell(..))
import Bomb16.View.Color as Color
import Color as C exposing (Color)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


viewPlayer : { food : Int } -> Element msg
viewPlayer { food } =
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
        , background = Nothing
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
                Just (Monster int) ->
                    Just <| Color.fromInt int

                Just (Wall _) ->
                    Just <| Color.black

                Just (Bomb _) ->
                    Just <| Color.red

                _ ->
                    Nothing
    in
    internal
        { text = text
        , background = color
        , dynamicSized =
            case maybeCell of
                Just (Monster _) ->
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
                            ([ Element.centerX
                             , Element.centerY
                             , Font.size
                                (if dynamicSized then
                                    120
                                        // (text
                                                |> Tuple.first
                                                |> String.length
                                                |> (+) 1
                                           )

                                 else
                                    40
                                )
                             ]
                                ++ (if dynamicSized then
                                        [ Font.family [ Font.serif ] ]

                                    else
                                        []
                                   )
                            )
                    ]

                ( primary, secondary ) ->
                    [ secondary
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Font.size 30
                            ]
                    , primary
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Font.family [ Font.serif ]
                            , Font.size
                                (if dynamicSized then
                                    30
                                        // (text
                                                |> Tuple.first
                                                |> String.length
                                           )

                                 else
                                    20
                                )
                            ]
                    ]
            )
                |> Element.column
                    ([ Element.width <| Element.px 75
                     , Element.height <| Element.px 75
                     , Element.padding 8
                     , Element.spaceEvenly
                     ]
                        ++ (background
                                |> Maybe.map
                                    (C.toRgba
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
