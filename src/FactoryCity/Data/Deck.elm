module FactoryCity.Data.Deck exposing
    ( Deck
    , Selected(..)
    , first
    , fromList
    , generator
    , moveTofirst
    , playFirst
    , playSecond
    , played
    , remaining
    , second
    , shuffle
    )

import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..), Item(..))
import Grid.Direction as Direction exposing (Direction(..))
import Jsonstore exposing (Json)
import List.Zipper as Zipper exposing (Zipper(..))
import Random exposing (Generator)
import Random.List as RandomList


type alias Deck =
    String 


init : Deck
init =
    [ CellType.crate Wood
    , CellType.crate Stone
    , CellType.furnace
    , CellType.belt { from = Up, to = Down }
    , CellType.merger Down
    , CellType.merger Left
    , CellType.output
    ]
        |> Array.fromList





second : Deck -> Maybe CellType
second =
    Zipper.after
        >> List.head


{-| Move the focus to the first element of the list.
-}
moveTofirst : Zipper a -> Zipper a
moveTofirst ((Zipper ls x rs) as zipper) =
    case List.reverse ls of
        [] ->
            zipper

        y :: ys ->
            Zipper [] y (ys ++ [ x ] ++ rs)


playFirst : { shuffle : Bool } -> Deck -> Generator Deck
playFirst options deck =
    case deck |> Zipper.next of
        Just newDeck ->
            Random.constant newDeck

        Nothing ->
            if options.shuffle then
                deck |> shuffle

            else
                generator


playSecond : Deck -> Deck
playSecond deck =
    case deck |> Zipper.after of
        b :: tail ->
            deck
                |> Zipper.mapBefore (\list -> [ b ] |> List.append list)
                |> Zipper.mapAfter (always tail)

        [] ->
            deck


shuffle : Deck -> Generator Deck
shuffle =
    Zipper.toList
        >> RandomList.shuffle
        >> Random.map fromList
