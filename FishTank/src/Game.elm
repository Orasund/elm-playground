module Game exposing (..)

import Config
import Dict exposing (Dict)
import Fish exposing (Fish, FishId)
import Random exposing (Generator)
import Set exposing (Set)
import Vector


type alias Random a =
    Generator a


type alias Game =
    { fish : Dict FishId Fish
    , nextFishId : FishId
    , locations : Dict FishId ( Float, Float )
    , directions : Dict FishId Float
    , goals : Dict FishId (Maybe ( Float, Float ))
    , tank : Dict ( Int, Int ) (Set FishId)
    , storage : Set FishId
    }


new : Game
new =
    { fish = Dict.empty
    , nextFishId = 0
    , locations = Dict.empty
    , directions = Dict.empty
    , goals = Dict.empty
    , tank = Dict.empty
    , storage = Set.empty
    }


randomLocation : Random ( Float, Float )
randomLocation =
    Random.pair (Random.float 1 (Config.tankWidth - 1))
        (Random.float 1 (Config.tankHeight - 1))


randomLocationAround : ( Float, Float ) -> Random ( Float, Float )
randomLocationAround location =
    randomLocation
        |> Random.map
            (\goal ->
                location
                    |> Vector.to goal
                    |> Vector.normalize
                    |> Vector.scaleBy 0.5
                    |> Vector.addTo location
            )


locationToPosition : ( Float, Float ) -> ( Int, Int )
locationToPosition ( x, y ) =
    ( floor x, floor y )


addFish : ( Float, Float ) -> Fish -> Game -> Game
addFish pos fish game =
    { game
        | fish = game.fish |> Dict.insert game.nextFishId fish
        , nextFishId = game.nextFishId + 1
    }
        |> insertIntoTank pos game.nextFishId


act : FishId -> Game -> Random Game
act fishId game =
    Maybe.map2
        (\currentLocation goal ->
            case goal of
                Nothing ->
                    randomLocation
                        |> Random.map
                            (\location ->
                                { game
                                    | goals = game.goals |> Dict.insert fishId (Just location)
                                    , directions =
                                        game.directions
                                            |> Dict.insert fishId
                                                (currentLocation |> Vector.to location |> Vector.angle)
                                }
                            )
                        |> Random.andThen (act fishId)

                Just location ->
                    currentLocation
                        |> Vector.to location
                        |> (\vector ->
                                (if Vector.length vector < 1 then
                                    Random.constant Nothing

                                 else
                                    randomLocationAround location
                                        |> Random.map
                                            (\newGoal ->
                                                Just newGoal
                                            )
                                )
                                    |> Random.map
                                        (\newGoal ->
                                            { game | goals = game.goals |> Dict.insert fishId newGoal }
                                                |> moveTo
                                                    (vector
                                                        |> Vector.normalize
                                                        |> Vector.scaleBy (1 / 3)
                                                        |> Vector.addTo currentLocation
                                                    )
                                                    fishId
                                        )
                           )
        )
        (Dict.get fishId game.locations)
        (Dict.get fishId game.goals)
        |> Maybe.withDefault (Random.constant game)


moveTo : ( Float, Float ) -> FishId -> Game -> Game
moveTo location fishId game =
    game.locations
        |> Dict.get fishId
        |> Maybe.map
            (\oldLocation ->
                game.tank
                    |> Dict.update (locationToPosition oldLocation)
                        (Maybe.map (Set.remove fishId))
                    |> Dict.update (locationToPosition location)
                        (\maybe ->
                            maybe
                                |> Maybe.withDefault Set.empty
                                |> Set.insert fishId
                                |> Just
                        )
                    |> (\tank ->
                            { game
                                | locations = game.locations |> Dict.insert fishId location
                                , directions =
                                    game.directions
                                        |> Dict.insert fishId
                                            (oldLocation
                                                |> Vector.to location
                                                |> Vector.angle
                                            )
                                , tank = tank
                            }
                       )
            )
        |> Maybe.withDefault game


breed : ( Float, Float ) -> ( FishId, FishId ) -> Game -> Random Game
breed pos ( fishId1, fishId2 ) game =
    Maybe.map2
        (\fish1 fish2 ->
            Fish.fromParents fish1 fish2
                |> Random.map (\fish -> addFish pos fish game)
        )
        (game.fish |> Dict.get fishId1)
        (game.fish |> Dict.get fishId2)
        |> Maybe.withDefault (Random.constant game)


removeFromTank : FishId -> Game -> Maybe Game
removeFromTank fishId game =
    game.locations
        |> Dict.get fishId
        |> Maybe.map
            (\location ->
                game.tank
                    |> Dict.update (locationToPosition location)
                        (Maybe.map (Set.remove fishId))
            )
        |> Maybe.map
            (\tank ->
                { game
                    | tank = tank
                    , directions = game.directions |> Dict.remove fishId
                    , locations = game.locations |> Dict.remove fishId
                    , goals = game.goals |> Dict.remove fishId
                }
            )


sellFish : FishId -> Game -> Game
sellFish fishId game =
    game
        |> removeFromTank fishId
        |> Maybe.withDefault game
        |> (\g ->
                { g
                    | storage = game.storage |> Set.remove fishId
                    , fish = game.fish |> Dict.remove fishId
                }
           )


insertIntoTank : ( Float, Float ) -> FishId -> Game -> Game
insertIntoTank pos fishId game =
    { game
        | locations = game.locations |> Dict.insert fishId pos
        , directions = game.directions |> Dict.insert fishId 0
        , goals = game.goals |> Dict.insert fishId Nothing
        , tank =
            game.tank
                |> Dict.update (locationToPosition pos)
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault Set.empty
                            |> Set.insert fishId
                            |> Just
                    )
    }


load : FishId -> Game -> Generator Game
load fishId game =
    randomLocation
        |> Random.map
            (\location ->
                { game | storage = game.storage |> Set.remove fishId }
                    |> insertIntoTank location fishId
            )


store : FishId -> Game -> Game
store fishId game =
    removeFromTank fishId game
        |> Maybe.map (\g -> { g | storage = game.storage |> Set.insert fishId })
        |> Maybe.withDefault game
