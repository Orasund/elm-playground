module Data.Card exposing (..)

import Gen.Enum.Face exposing (Face)
import Gen.Record.Card


type alias Card =
    Gen.Record.Card.Card


fromFace : Face -> Card
fromFace face =
    { face = face, amount = 1 }
