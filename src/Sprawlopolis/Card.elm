module Sprawlopolis.Card exposing (normalView, view)

import Sprawlopolis.Box as Box exposing (BoxContent(..))
import Sprawlopolis.Color as Color exposing (Color(..))
import Sprawlopolis.View as View
import Svg exposing (Svg)


normalView : List Color -> List (Svg msg)
normalView list =
    case list of
        [ G as b1, b2, b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content = Nothing
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content = Just <| VStreet
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Just <| SWCurve
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Just <| VStreet
                }
            ]
                |> List.concat

        [ b1, b2, G as b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content = Just <| HStreet
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content = Just <| HStreet
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Nothing
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Just <| SECurve
                }
            ]
                |> List.concat

        [ b1, G as b2, b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content = Just <| NWCurve
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content = Nothing
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Just <| HStreet
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Just <| HStreet
                }
            ]
                |> List.concat

        [ b1, b2, b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content = Just <| VStreet
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content = Just <| NECurve
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Just <| VStreet
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Nothing
                }
            ]
                |> List.concat

        _ ->
            []


view : List Color -> List (Svg msg)
view list =
    case list of
        [ G as b1, b2, b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content =
                    if b3 == Y then
                        Just <|
                            Rule <|
                                if b2 == B then
                                    1

                                else
                                    2

                    else
                        Nothing
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content = Just <| VStreet
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Just <| HStreet
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Just <| NWCurve
                }
            ]
                |> List.concat

        [ b1, b2, G as b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content = Just <| NWCurve
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content =
                    Just <|
                        Rule <|
                            if b1 == Y then
                                if b2 == B then
                                    5

                                else
                                    6

                            else if b1 == R then
                                if b2 == B then
                                    7

                                else
                                    8

                            else if b2 == R then
                                13

                            else
                                14
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Nothing
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Just <| SECurve
                }
            ]
                |> List.concat

        [ b1, G as b2, b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content = Just <| VStreet
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content =
                    if b3 == Y then
                        Just <|
                            Rule <|
                                if b1 == B then
                                    3

                                else
                                    4

                    else
                        Nothing
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Just <| NECurve
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Just <| HStreet
                }
            ]
                |> List.concat

        [ b1, b2, b3, b4 ] ->
            [ Box.draw
                { color = b1
                , withXOffset = False
                , withYOffset = False
                , content =
                    Just <|
                        Rule <|
                            if b1 == Y then
                                if b2 == B then
                                    9

                                else
                                    10

                            else if b1 == R then
                                if b2 == B then
                                    11

                                else
                                    12

                            else if b2 == R then
                                15

                            else
                                16
                }
            , Box.draw
                { color = b2
                , withXOffset = True
                , withYOffset = False
                , content = Just <| NECurve
                }
            , Box.draw
                { color = b3
                , withXOffset = False
                , withYOffset = True
                , content = Just <| SWCurve
                }
            , Box.draw
                { color = b4
                , withXOffset = True
                , withYOffset = True
                , content = Nothing
                }
            ]
                |> List.concat

        _ ->
            []
