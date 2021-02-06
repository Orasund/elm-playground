module Farmig.Data.Cell exposing (Cell(..), toString)

import Color exposing (Color)
import Farmig.Data.Food as Food exposing (Food)
import Farmig.Data.Item as Item exposing (Item)


type Cell
    = Ground
    | Goal
    | Food Food
    | Item Item
    | Seed Food
    | Plant Int Food
    | Wood
    | Rabbit


toString : Cell -> ( String, String )
toString cell =
    case cell of
        Ground ->
            ( "", "" )

        Goal ->
            ( "⭐", "" )

        Food food ->
            ( Food.toString food, String.fromInt <| Food.value food )

        Item item ->
            ( Item.toString item, "" )

        Seed food ->
            ( "🌱", Food.toString food )

        Plant int food ->
            ( String.fromInt int, Food.toString food )

        Wood ->
            ( "\u{1FAB5}", "" )

        Rabbit ->
            ( "🐰", "" )

