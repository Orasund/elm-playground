module View.Stack exposing (..)

import Game.Stack
import Html exposing (Html)
import Html.Attributes
import Random
import View.Component


singleCard : Html msg
singleCard =
    [ Game.Stack.item ()
        |> (\it -> { it | rotation = -pi / 16, movement = ( -50, 0 ) })
    ]
        |> Game.Stack.toHtml []
            { view = \_ () -> View.Component.defaultCard
            , empty = View.Component.empty []
            }


below : Html msg
below =
    Game.Stack.item ()
        |> List.repeat 3
        |> Game.Stack.withMovement
            { minDistance = -50
            , maxDistance = 0
            , minAngle = pi / 2
            , maxAngle = pi / 2
            }
        |> Game.Stack.toHtml []
            { view = \_ () -> View.Component.defaultCard
            , empty = View.Component.empty []
            }


rotated : Html msg
rotated =
    Game.Stack.item ()
        |> List.repeat 3
        |> Game.Stack.withRotation { min = -pi / 16, max = 0 }
        |> Game.Stack.toHtml []
            { view = \_ () -> View.Component.defaultCard
            , empty = View.Component.empty []
            }


random : Html msg
random =
    ()
        |> List.repeat 3
        |> Game.Stack.generate
            (Random.map2
                (\rotationFun moveFun a b ->
                    { rotation = (rotationFun a b).rotation
                    , movement = (moveFun a b).movement
                    }
                )
                (Game.Stack.randomRotation { min = -pi / 8, max = pi / 8 })
                (Game.Stack.randomMovement { minAngle = -pi / 8, maxAngle = pi / 8, minDistance = -50, maxDistance = 50 })
            )
        |> (\generator -> Random.step generator (Random.initialSeed 40))
        |> Tuple.first
        |> Game.Stack.toHtml []
            { view = \_ () -> View.Component.defaultCard
            , empty = View.Component.empty []
            }


hand : Html msg
hand =
    Game.Stack.item ()
        |> List.repeat 5
        |> Game.Stack.withMovement
            { minDistance = -100
            , maxDistance = 100
            , minAngle = -pi / 32
            , maxAngle = pi / 32
            }
        |> Game.Stack.withRotation { min = -pi / 16, max = pi / 16 }
        |> List.indexedMap
            (\i stackItem ->
                if i == 3 then
                    { stackItem
                        | rotation = 0
                        , movement =
                            stackItem.movement
                                |> Tuple.mapBoth
                                    ((+) 0)
                                    ((+) -50)
                    }

                else
                    stackItem
            )
        |> Game.Stack.toHtml
            [ Html.Attributes.style "height" "250px"
            , Html.Attributes.style "width" "400px"
            , Html.Attributes.style "align-items" "end"
            , Html.Attributes.style "justify-content" "center"
            ]
            { view = \_ () -> View.Component.defaultCard
            , empty = Html.text ""
            }
