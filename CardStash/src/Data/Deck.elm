module Data.Deck exposing (..)

import Data.Card as Card
import Gen.Enum.Face exposing (Face(..))
import Gen.Record.Card exposing (Card)
import Random exposing (Generator)
import Random.List


type alias Deck =
    List ( Card, Bool )


amountPerFace : Int
amountPerFace =
    8


generator : Generator ( Deck, Deck )
generator =
    Gen.Enum.Face.asList
        |> List.concatMap (\face -> List.repeat amountPerFace face)
        |> Random.List.shuffle
        |> Random.map
            (\list ->
                let
                    amount =
                        List.length list // 2
                in
                ( list |> List.take amount |> fromList
                , list |> List.drop amount |> fromList
                )
            )


fromList : List Face -> Deck
fromList list =
    case list of
        head :: tail ->
            ( Card.fromFace head, True ) :: (tail |> List.map (\face -> ( Card.fromFace face, False )))

        [] ->
            []
