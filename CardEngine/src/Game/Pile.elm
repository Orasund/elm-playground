module Game.Pile exposing (..)

import Game.Card
import Html exposing (Attribute, Html)
import Html.Attributes
import Random exposing (Generator)


type alias PileItem a =
    { movement : ( Float, Float )
    , rotation : Float
    , zIndex : Int
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
    -> Generator (List (PileItem a))
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
            (List.indexedMap
                (\i ( a, args ) ->
                    { movement = args.movement
                    , rotation = args.rotation
                    , card = a
                    , zIndex = i + 1
                    }
                )
            )


item : a -> PileItem a
item a =
    { movement = ( 0, 0 ), rotation = 0, card = a, zIndex = 1 }


map : (a -> b) -> PileItem a -> PileItem b
map fun i =
    { movement = i.movement
    , rotation = i.rotation
    , card = fun i.card
    , zIndex = i.zIndex
    }


mapMovement : (( Float, Float ) -> ( Float, Float )) -> PileItem a -> PileItem a
mapMovement fun stackItem =
    { stackItem | movement = fun stackItem.movement }


mapRotation : (Float -> Float) -> PileItem a -> PileItem a
mapRotation fun stackItem =
    { stackItem | rotation = fun stackItem.rotation }


mapZIndex : (Int -> Int) -> PileItem a -> PileItem a
mapZIndex fun stackItem =
    { stackItem | zIndex = fun stackItem.zIndex }


withDependentRotation : (Int -> a -> Float) -> List (PileItem a) -> List (PileItem a)
withDependentRotation fun =
    List.indexedMap (\i stackItem -> { stackItem | rotation = fun i stackItem.card })


withDependentMovement : (Int -> a -> ( Float, Float )) -> List (PileItem a) -> List (PileItem a)
withDependentMovement fun =
    List.indexedMap (\i stackItem -> { stackItem | movement = fun i stackItem.card })


withRotation : { min : Float, max : Float } -> List (PileItem a) -> List (PileItem a)
withRotation args list =
    withDependentRotation
        (\i _ ->
            args.min + toFloat i * (args.max - args.min) / toFloat (List.length list - 1)
        )
        list


withMovement : { minAngle : Float, maxAngle : Float, minDistance : Float, maxDistance : Float } -> List (PileItem a) -> List (PileItem a)
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
    -> List (PileItem a)
    -> Html msg
toHtml attrs { view, empty } stack =
    stack
        |> List.indexedMap
            (\i it ->
                view i
                    it.card
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "z-index" (String.fromInt it.zIndex)
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
