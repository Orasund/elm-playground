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
                , content = Nothing
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
                , content = Nothing
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
                , content = Nothing
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
