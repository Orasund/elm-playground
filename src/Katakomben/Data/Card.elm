module Katakomben.Data.Card exposing (Card(..), Level(..))

import Element exposing (Color)
import Framework.Color as Color
import Katakomben.Data.Item exposing (Item, ItemSort(..))
import Katakomben.Data.Monster exposing (Monster)


type Level
    = CatacombsOfDunkelhall
    | GraveyardChapel


type Card
    = Entrance Level
    | Tomb Level
    | Death
    | Loot Item
    | Enemy Monster
    | Camp
    | Shrine Level
