module Game.Stack exposing (..)

import Game.Card
import Html exposing (Attribute, Html)
import Html.Attributes
import Random exposing (Generator)


type alias StackItem a =
    { movement : ( Float, Float )
    , rotation : Float
    , card : a
    }


randomRotation :
    { min : Float, max : Float }
    -> Generator (Int -> a -> { movement : ( Float, Float ), rotation : Float })
randomRotation args =
    Random.map
        (\rotation _ _ ->
            { movement = ( 0, 0 ), rotation = rotation }
        )
        (Random.float args.min args.max)


randomMovement :
    { minAngle : Float, maxAngle : Float, minDistance : Float, maxDistance : Float }
    -> Generator (Int -> a -> { movement : ( Float, Float ), rotation : Float })
randomMovement args =
    Random.map2
        (\rotation distance _ _ ->
            { movement = fromPolar ( distance, rotation ), rotation = rotation }
        )
        (Random.float args.minAngle args.maxAngle)
        (Random.float args.minDistance args.maxDistance)


generate :
    Generator (Int -> a -> { movement : ( Float, Float ), rotation : Float })
    -> List a
    -> Generator (List (StackItem a))
generate fun list =
    Random.list (List.length list)
        fun
        |> Random.map
            (\randomList ->
                randomList
                    |> List.map2 Tuple.pair list
                    |> List.indexedMap
                        (\i ( a, f ) ->
                            f i a
                                |> Tuple.pair a
                        )
            )
        |> Random.map
            (List.map
                (\( a, args ) ->
                    { movement = args.movement
                    , rotation = args.rotation
                    , card = a
                    }
                )
            )


item : a -> StackItem a
item a =
    { movement = ( 0, 0 ), rotation = 0, card = a }


map : (a -> b) -> StackItem a -> StackItem b
map fun i =
    { movement = i.movement
    , rotation = i.rotation
    , card = fun i.card
    }


withDependentRotation : (Int -> a -> Float) -> List (StackItem a) -> List (StackItem a)
withDependentRotation fun =
    List.indexedMap (\i stackItem -> { stackItem | rotation = fun i stackItem.card })


withDependentMovement : (Int -> a -> ( Float, Float )) -> List (StackItem a) -> List (StackItem a)
withDependentMovement fun =
    List.indexedMap (\i stackItem -> { stackItem | movement = fun i stackItem.card })


withRotation : { min : Float, max : Float } -> List (StackItem a) -> List (StackItem a)
withRotation args list =
    withDependentRotation
        (\i _ ->
            args.min + toFloat i * (args.max - args.min) / toFloat (List.length list - 1)
        )
        list


withMovement : { minAngle : Float, maxAngle : Float, minDistance : Float, maxDistance : Float } -> List (StackItem a) -> List (StackItem a)
withMovement args list =
    withDependentMovement
        (\i _ ->
            ( args.minDistance + toFloat i * (args.maxDistance - args.minDistance) / toFloat (List.length list - 1)
            , args.minAngle + toFloat i * (args.maxAngle - args.minAngle) / toFloat (List.length list - 1)
            )
                |> fromPolar
        )
        list


toHtml :
    List (Attribute msg)
    ->
        { view : Int -> a -> List (Attribute msg) -> Html msg
        , empty : Html msg
        }
    -> List (StackItem a)
    -> Html msg
toHtml attrs { view, empty } stack =
    stack
        |> List.indexedMap
            (\i it ->
                view i
                    it.card
                    [ Html.Attributes.style "position" "absolute"
                    , Game.Card.transform
                        [ Game.Card.move it.movement
                        , Game.Card.rotate it.rotation
                        ]
                    ]
            )
        |> (::) empty
        |> Html.div
            ([ Html.Attributes.style "display" "flex"
             , Html.Attributes.style "position" "relative"
             ]
                ++ attrs
            )
