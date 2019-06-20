module AsteroidMiner.Building.Merger exposing (canStore, update)

import AsteroidMiner.Building as Building exposing (BuildingType(..))
import AsteroidMiner.Data.Item exposing (Item)
import AsteroidMiner.Data.Map exposing (Command, Neighborhood)
import AsteroidMiner.Lib.Command as Command
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import Grid.Direction exposing (Direction)


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ input { value, item } =
    False


update : Neighborhood -> Command
update neigh =
    neigh
        |> Neighborhood.toList
        |> List.filterMap
            (\( dir, a ) ->
                case a of
                    Just (ColoredConveyorBelt _ d) ->
                        if dir == d then
                            Just <| Command.send dir

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> Command.batch
