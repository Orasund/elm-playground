module View.Stack exposing (..)

import CardGame.Card
import CardGame.Stack
import Html exposing (Html)
import Html.Attributes
import Random
import View.Component


singleCard : Html msg
singleCard =
    [ CardGame.Stack.item ()
        |> (\it -> { it | rotation = pi / 16, movement = ( 50, 0 ) })
    ]
        |> CardGame.Stack.toHtml [] (\() -> View.Component.defaultCard) (CardGame.Card.empty "Empty Stack")


below : Html msg
below =
    CardGame.Stack.item ()
        |> List.repeat 3
        |> CardGame.Stack.withMovement
            { minDistance = -50
            , maxDistance = 0
            , minAngle = pi / 2
            , maxAngle = pi / 2
            }
        |> CardGame.Stack.toHtml []
            (\() -> View.Component.defaultCard)
            (CardGame.Card.empty "Empty Stack")


rotated : Html msg
rotated =
    CardGame.Stack.item ()
        |> List.repeat 3
        |> CardGame.Stack.withRotation { min = -pi / 16, max = 0 }
        |> CardGame.Stack.toHtml []
            (\() -> View.Component.defaultCard)
            (CardGame.Card.empty "Empty Stack")


random : Html msg
random =
    ()
        |> List.repeat 3
        |> CardGame.Stack.generate
            (Random.map2
                (\rotationFun moveFun a b ->
                    { rotation = (rotationFun a b).rotation
                    , movement = (moveFun a b).movement
                    }
                )
                (CardGame.Stack.randomRotation { min = -pi / 8, max = pi / 8 })
                (CardGame.Stack.randomMovement { minAngle = -pi / 8, maxAngle = pi / 8, minDistance = -50, maxDistance = 50 })
            )
        |> (\generator -> Random.step generator (Random.initialSeed 40))
        |> Tuple.first
        |> CardGame.Stack.toHtml []
            (\() -> View.Component.defaultCard)
            (CardGame.Card.empty "Empty Stack")


hand : Html msg
hand =
    CardGame.Stack.item ()
        |> List.repeat 5
        |> CardGame.Stack.withMovement
            { minDistance = -100
            , maxDistance = 100
            , minAngle = -pi / 32
            , maxAngle = pi / 32
            }
        |> CardGame.Stack.withRotation { min = -pi / 16, max = pi / 16 }
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
        |> CardGame.Stack.toHtml
            [ Html.Attributes.style "height" "250px"
            , Html.Attributes.style "width" "400px"
            , Html.Attributes.style "align-items" "end"
            , Html.Attributes.style "justify-content" "center"
            ]
            (\() -> View.Component.defaultCard)
            (Html.text "")
