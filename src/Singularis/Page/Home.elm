module Singularis.Page.Home exposing (route, view)

import Element exposing (Element)
import Singularis.Page as Page exposing (Route(..))
import Singularis.View.Element as Element
import Singularis.View.Polygon as Polygon


route : Route
route =
    Home


view : Element msg
view =
    Element.column [ Element.centerX] <|
        [ Element.title "Primer Singularis"
        , Element.row
            [ Element.width <| Element.fill
            , Element.spaceEvenly
            , Element.centerX
            ]
          <|
            [ Element.html <| Polygon.view 3
            , Element.html <| Polygon.view 4
            , Element.html <| Polygon.view 6
            ]
        ]
