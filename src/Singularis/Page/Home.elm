module Singularis.Page.Home exposing (view)

import Element exposing (Element)
import Singularis.Page as Page exposing (Route(..))
import Singularis.View.Element as Element
import Singularis.View.Polygon as Polygon


view : Float -> Element msg
view scale =
    Element.column [ Element.centerX ] <|
        [ Element.title scale <| "Occultus Singularis"
        , Element.row
            [ Element.width <| Element.fill
            , Element.spaceEvenly
            , Element.centerX
            ]
          <|
            [ Element.html <| Polygon.view scale <| 3
            , Element.html <| Polygon.view scale <| 4
            , Element.html <| Polygon.view scale <| 6
            ]
        ]
