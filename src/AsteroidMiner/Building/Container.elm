module AsteroidMiner.Building.Container exposing (canStore, update)

import AsteroidMiner.Data exposing (maxValue)
import AsteroidMiner.Data.Map exposing (Command, Item, Neighborhood)
import AsteroidMiner.Lib.Command as Command


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ input { value, item } =
    (input == item) && (value < maxValue)


update : Neighborhood -> Command
update _ =
    Command.idle
