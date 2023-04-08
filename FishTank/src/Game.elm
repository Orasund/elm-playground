module Game exposing (..)

import Array
import Config
import Dict exposing (Dict)
import Fish exposing (Fish, FishId)
import Random exposing (Generator)
import Set exposing (Set)
import Vector


type alias Random a =
    Generator a


type alias TankId =
    Int


type alias FoodId =
    Int


type Goal
    = Idle
    | MoveTo ( Float, Float )


type alias Game =
    { fish : Dict FishId Fish
    , nextFishId : FishId
    , locations : Dict FishId ( Float, Float )
    , directions : Dict FishId Float
    , goals : Dict FishId Goal
    , tanks : Dict TankId (Dict ( Int, Int ) (Set FishId))
    , storage : Set FishId
    , fed : Set FishId
    , food : Dict FoodId ( Float, Float )
    }


new : Game
new =
    { fish = Dict.empty
    , nextFishId = 0
    , locations = Dict.empty
    , directions = Dict.empty
    , goals = Dict.empty
    , tanks =
        [ ( 0, Dict.empty )
        , ( 1, Dict.empty )
        ]
            |> Dict.fromList
    , storage = Set.empty
    , fed = Set.empty
    , food = Dict.empty
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


addFish : ( Float, Float ) -> TankId -> Fish -> Game -> Game
addFish pos tankId fish game =
    { game
        | fish = game.fish |> Dict.insert game.nextFishId fish
        , nextFishId = game.nextFishId + 1
    }
        |> insertIntoTank pos tankId game.nextFishId


pickTwo : List a -> Random (Maybe ( a, a ))
pickTwo list =
    let
        arr =
            Array.fromList list
    in
    Random.map2
        (\i1 i2 ->
            Maybe.map2 Tuple.pair
                (Array.get
                    (if i1 > i2 then
                        i2

                     else
                        i2 + 1
                    )
                    arr
                )
                (Array.get i1 arr)
        )
        (Random.int 0 (List.length list - 1))
        (Random.int 0 (List.length list - 2))


tryMating : TankId -> FishId -> Game -> Generator Game
tryMating tankId fishId game =
    if Set.member fishId game.fed then
        game.tanks
            |> Dict.get tankId
            |> Maybe.map Dict.values
            |> Maybe.map (List.concatMap Set.toList)
            |> Maybe.map Set.fromList
            |> Maybe.map (Set.intersect game.fed)
            |> Maybe.withDefault Set.empty
            |> Set.remove fishId
            |> Set.toList
            |> (\list ->
                    case list of
                        head :: tail ->
                            Random.uniform head tail
                                |> Random.map Just

                        [] ->
                            Random.constant Nothing
               )
            |> Random.andThen
                (\maybe ->
                    maybe
                        |> Maybe.map
                            (\fish ->
                                game
                                    |> breed tankId ( fishId, fish )
                            )
                        |> Maybe.withDefault (Random.constant game)
                )

    else
        Random.constant game


act : TankId -> FishId -> Game -> Random Game
act tankId fishId game =
    Maybe.map2
        (\currentLocation goal ->
            case goal of
                Idle ->
                    randomLocation
                        |> Random.map
                            (\location ->
                                { game
                                    | goals = game.goals |> Dict.insert fishId (MoveTo location)
                                    , directions =
                                        game.directions
                                            |> Dict.insert fishId
                                                (currentLocation |> Vector.to location |> Vector.angle)
                                }
                            )
                        |> Random.andThen (act tankId fishId)

                MoveTo location ->
                    currentLocation
                        |> Vector.to location
                        |> (\vector ->
                                (if Vector.length vector < 1 then
                                    game.goals
                                        |> Dict.insert fishId Idle

                                 else
                                    game.goals
                                )
                                    |> (\goals ->
                                            { game | goals = goals }
                                                |> moveTo
                                                    tankId
                                                    (vector
                                                        |> Vector.normalize
                                                        |> Vector.scaleBy (1 / 3)
                                                        |> Vector.addTo currentLocation
                                                    )
                                                    fishId
                                       )
                           )
                        |> Random.constant
        )
        (Dict.get fishId game.locations)
        (Dict.get fishId game.goals)
        |> Maybe.withDefault (Random.constant game)


moveTo : TankId -> ( Float, Float ) -> FishId -> Game -> Game
moveTo tankId location fishId game =
    game.locations
        |> Dict.get fishId
        |> Maybe.map
            (\oldLocation ->
                game.tanks
                    |> Dict.update tankId
                        (Maybe.map
                            (\maybeTank ->
                                maybeTank
                                    |> Dict.update (locationToPosition oldLocation)
                                        (Maybe.map (Set.remove fishId))
                                    |> Dict.update (locationToPosition location)
                                        (\maybe ->
                                            maybe
                                                |> Maybe.withDefault Set.empty
                                                |> Set.insert fishId
                                                |> Just
                                        )
                            )
                        )
                    |> (\tanks ->
                            { game
                                | locations = game.locations |> Dict.insert fishId location
                                , directions =
                                    game.directions
                                        |> Dict.insert fishId
                                            (oldLocation
                                                |> Vector.to location
                                                |> Vector.angle
                                            )
                                , tanks = tanks
                            }
                       )
            )
        |> Maybe.withDefault game


breed : TankId -> ( FishId, FishId ) -> Game -> Random Game
breed tankId ( fishId1, fishId2 ) game =
    Maybe.map3
        (\location fish1 fish2 ->
            Fish.fromParents fish1 fish2
                |> Random.map
                    (\fish ->
                        { game
                            | fed =
                                game.fed
                                    |> Set.remove fishId1
                                    |> Set.remove fishId2
                        }
                            |> addFish location tankId fish
                    )
        )
        (game.locations |> Dict.get fishId1)
        (game.fish |> Dict.get fishId1)
        (game.fish |> Dict.get fishId2)
        |> Maybe.withDefault (Random.constant game)


removeFromTank : TankId -> FishId -> Game -> Game
removeFromTank tankId fishId game =
    game.locations
        |> Dict.get fishId
        |> Maybe.map
            (\location ->
                { game
                    | tanks =
                        game.tanks
                            |> Dict.update tankId
                                (Maybe.map
                                    (Dict.update (locationToPosition location)
                                        (Maybe.map (Set.remove fishId))
                                    )
                                )
                }
            )
        |> Maybe.withDefault game


insertIntoTank : ( Float, Float ) -> TankId -> FishId -> Game -> Game
insertIntoTank pos tankId fishId game =
    { game
        | locations = game.locations |> Dict.insert fishId pos
        , directions = game.directions |> Dict.insert fishId 0
        , goals = game.goals |> Dict.insert fishId Idle
        , tanks =
            game.tanks
                |> Dict.update tankId
                    (Maybe.map
                        (Dict.update (locationToPosition pos)
                            (\maybe ->
                                maybe
                                    |> Maybe.withDefault Set.empty
                                    |> Set.insert fishId
                                    |> Just
                            )
                        )
                    )
    }


sellFish : TankId -> FishId -> Game -> Game
sellFish tankId fishId game =
    game
        |> removeFromTank tankId fishId
        |> (\g ->
                { g
                    | storage = game.storage |> Set.remove fishId
                    , fish = game.fish |> Dict.remove fishId
                }
           )


load : TankId -> FishId -> Game -> Generator Game
load tankId fishId game =
    randomLocation
        |> Random.map
            (\location ->
                { game | storage = game.storage |> Set.remove fishId }
                    |> insertIntoTank location tankId fishId
            )


store : TankId -> FishId -> Game -> Game
store tankId fishId game =
    removeFromTank tankId fishId game
        |> (\g -> { g | storage = game.storage |> Set.insert fishId })


feedTank : TankId -> Game -> Game
feedTank tankId game =
    { game
        | fed =
            game.tanks
                |> Dict.get tankId
                |> Maybe.withDefault Dict.empty
                |> Dict.values
                |> List.concatMap Set.toList
                |> Set.fromList
                |> Set.union game.fed
    }



{--addFood : TankId -> Game -> Random Game
addFood tankId game =
    randomLocation
        |> Random.map
            (\location ->
                game.food |> Dict
            )--}
