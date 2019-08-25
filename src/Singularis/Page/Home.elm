module Singularis.Page.Home exposing (view)

import Dict exposing (Dict)
import Element exposing (Element)
import Singularis.Page as Page exposing (Route(..))
import Singularis.View.Element as Element
import Singularis.View.Polygon as Polygon


view : Float -> Dict String (Element msg)
view scale =
    Dict.fromList <|
        [ ( "elements"
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
          )
        ]
