module Game.Pile exposing (..)

import Game.Card
import Game.Entity exposing (Entity)
import Html exposing (Attribute, Html)
import Html.Attributes
import Random exposing (Generator)


type alias PileItem a =
    Entity a


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
                    Game.Entity.new a
                        |> Game.Entity.withZIndex (i + 1)
                        |> Game.Entity.withPosition args.movement
                        |> Game.Entity.withRotation args.rotation
                )
            )


item : a -> PileItem a
item =
    Game.Entity.new


map : (a -> b) -> PileItem a -> PileItem b
map =
    Game.Entity.map


mapMovement : (( Float, Float ) -> ( Float, Float )) -> PileItem a -> PileItem a
mapMovement =
    Game.Entity.mapPosition


mapRotation : (Float -> Float) -> PileItem a -> PileItem a
mapRotation =
    Game.Entity.mapRotation


mapZIndex : (Int -> Int) -> PileItem a -> PileItem a
mapZIndex =
    Game.Entity.mapZIndex


withDependentRotation : (Int -> a -> Float) -> List (PileItem a) -> List (PileItem a)
withDependentRotation fun =
    List.indexedMap (\i entity -> entity |> Game.Entity.withRotation (fun i entity.content))


withDependentMovement : (Int -> a -> ( Float, Float )) -> List (PileItem a) -> List (PileItem a)
withDependentMovement fun =
    List.indexedMap (\i entity -> entity |> Game.Entity.withPosition (fun i entity.content))


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
                    it.content
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "z-index" (String.fromInt it.zIndex)
                    , Game.Card.transform
                        [ Game.Card.move it.position
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
