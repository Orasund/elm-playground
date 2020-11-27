module LittleWorldPuzzler.Data.Deck exposing
    ( Deck
    , Selected(..)
    , first
    , fromList
    , generator
    , json
    , moveTofirst
    , placeOnDiscard
    , playFirst
    , playSecond
    , played
    , remaining
    , second
    , shuffle
    )

import Jsonstore exposing (Json)
import List.Zipper as Zipper exposing (Zipper)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import Random exposing (Generator)
import Random.List as RandomList


type Selected
    = First
    | Second


type alias Deck =
    Zipper CellType


generator : Generator Deck
generator =
    [ Wood
    , Wood
    , Wood
    , Wood
    , Water
    , Water
    , Water
    , Water
    , Stone
    , Fire
    ]
        |> fromList
        |> shuffle


fromList : List CellType -> Deck
fromList =
    Zipper.fromList
        >> Zipper.withDefault Wood


remaining : Deck -> List CellType
remaining =
    Zipper.after


played : Deck -> List CellType
played =
    Zipper.before


first : Deck -> CellType
first =
    Zipper.current


second : Deck -> Maybe CellType
second =
    Zipper.after
        >> List.head


{-| Move the focus to the first element of the list.
-}
moveTofirst : Zipper a -> Zipper a
moveTofirst =
    Zipper.first



{--moveTofirst : Zipper a -> Zipper a
moveTofirst ((Zipper ls x rs) as zipper) =
    case List.reverse ls of
        [] ->
            zipper

        y :: ys ->
            Zipper [] y (List.concat [ ys, [ x ], rs ])--}


playFirst : Bool -> Deck -> Generator Deck
playFirst optionShuffle deck =
    case deck |> Zipper.after of
        b :: tail ->
            deck
                |> Zipper.mapCurrent (always b)
                |> Zipper.mapAfter (always tail)
                |> Random.constant

        [] ->
            generator
                |> (if optionShuffle then
                        Random.andThen shuffle

                    else
                        identity
                   )


playSecond : Deck -> Deck
playSecond deck =
    case deck |> Zipper.after of
        b :: tail ->
            deck
                --|> Zipper.mapBefore (\list -> [ b ] |> List.append list)
                |> Zipper.mapAfter (always tail)

        [] ->
            deck


placeOnDiscard : CellType -> Deck -> Deck
placeOnDiscard cellType deck =
    deck |> Zipper.mapAfter ((::) cellType)


shuffle : Deck -> Generator Deck
shuffle =
    Zipper.toList
        >> RandomList.shuffle
        >> Random.map fromList



{------------------------
   Json
------------------------}


json : Json Deck
json =
    Jsonstore.object
        (\remainingD firstD playedD ->
            Zipper.singleton firstD
                |> Zipper.mapBefore (always playedD)
                |> Zipper.mapAfter (always remainingD)
        )
        |> Jsonstore.withList "remaining" CellType.json remaining
        |> Jsonstore.with "first" CellType.json first
        |> Jsonstore.withList "played" CellType.json played
        |> Jsonstore.toJson
