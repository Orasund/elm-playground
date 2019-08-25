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
                [ Element.column []
                    [ Element.html <| Polygon.view scale <| 3
                    , Element.el [ Element.centerX ] <| Element.text "Spirit"
                    ]
                , Element.column []
                    [ Element.html <| Polygon.view scale <| 4
                    , Element.el [ Element.centerX ] <| Element.text "Matter"
                    ]
                , Element.column []
                    [ Element.html <| Polygon.view scale <| 6
                    , Element.el [ Element.centerX ] <| Element.text "Knowledge"
                    ]
                ]
          )
        ]
