module Game exposing (..)

import Action exposing (Action)
import Array
import Cat exposing (Cat)
import Config
import Dict exposing (Dict)
import Fish
import Fish.Common exposing (Breed, BreedId, Fish, FishId)
import Fish.Name
import Pigment exposing (Pigment, yellow)
import Random exposing (Generator)
import Set exposing (Set)
import Tank exposing (Tank)
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


type alias Order =
    { primary : Pigment
    , secondary : Pigment
    , cat : Cat
    }


type alias Game =
    { fish : Dict FishId Fish
    , nextFishId : FishId
    , directions : Dict FishId Float
    , goals : Dict FishId Goal
    , tanks : Dict TankId Tank
    , storage : Set FishId
    , fed : Set FishId
    , nextFoodId : FoodId
    , money : Int
    , breeds : Dict BreedId Breed
    , assignedBreed : Dict FishId BreedId
    , order : Order
    }


new : Game
new =
    { fish = Dict.empty
    , nextFishId = 0
    , directions = Dict.empty
    , goals = Dict.empty
    , tanks =
        [ ( 0, Tank.empty )
        ]
            |> Dict.fromList
    , storage = Set.empty
    , fed = Set.empty
    , nextFoodId = 0
    , money = Config.fishCost * 2
    , breeds = Dict.empty
    , assignedBreed = Dict.empty
    , order =
        { primary = { red = True, yellow = True, blue = False }
        , secondary = { red = True, yellow = True, blue = False }
        , cat = Cat.default
        }
    }


randomLocation : Random ( Float, Float )
randomLocation =
    Random.pair (Random.float 0 Config.tankWidth)
        (Random.float 0 Config.tankHeight)


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


addFish : ( Float, Float ) -> TankId -> Fish -> Game -> ( Game, FishId )
addFish pos tankId fish game =
    ( { game
        | fish = game.fish |> Dict.insert game.nextFishId fish
        , nextFishId = game.nextFishId + 1
      }
        |> insertIntoTank pos tankId game.nextFishId
    , game.nextFishId
    )


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


tryMating : TankId -> FishId -> Game -> Generator ( Game, List Action )
tryMating tankId fishId game =
    let
        tank =
            game.tanks |> Dict.get tankId

        fishCount =
            tank
                |> Maybe.map Tank.getFish
                |> Maybe.map Dict.size
                |> Maybe.withDefault 0
    in
    if Set.member fishId game.fed && fishCount < Config.maxFishPerTank then
        tank
            |> Maybe.map (Tank.getFishCell fishId)
            |> Maybe.map .fish
            |> Maybe.withDefault Set.empty
            |> Set.intersect game.fed
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
                                    |> mate tankId ( fishId, fish )
                            )
                        |> Maybe.withDefault (Random.constant ( game, [] ))
                )

    else
        Random.constant ( game, [] )


tryEating : TankId -> FishId -> Game -> Maybe Game
tryEating tankId fishId game =
    game.tanks
        |> Dict.get tankId
        |> Maybe.andThen
            (\tank ->
                tank
                    |> Tank.getFishLocation fishId
                    |> Maybe.map (Tuple.pair tank)
            )
        |> Maybe.map
            (\( tank, location ) ->
                tank
                    |> Tank.getCell location
            )
        |> Maybe.andThen
            (\cell ->
                cell.food
                    |> Set.toList
                    |> List.head
            )
        |> Maybe.map
            (\foodId ->
                game |> eat { tankId = tankId, foodId = foodId, fishId = fishId }
            )


eat :
    { tankId : TankId
    , foodId : FoodId
    , fishId : FishId
    }
    -> Game
    -> Game
eat args game =
    { game
        | tanks =
            game.tanks
                |> Dict.update args.tankId
                    (Maybe.map (Tank.removeFood args.foodId))
        , fed = game.fed |> Set.insert args.fishId
        , fish =
            game.fish
                |> Dict.update args.fishId
                    (Maybe.map (\fish -> { fish | size = fish.size + 1 }))
    }


startMoving : TankId -> FishId -> Game -> Random Game
startMoving tankId fishId game =
    game.tanks
        |> Dict.get tankId
        |> Maybe.andThen
            (\tank ->
                tank
                    |> Tank.getFishLocation fishId
                    |> Maybe.map
                        (\currentLocation ->
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
                        )
            )
        |> Maybe.withDefault (Random.constant game)


act : TankId -> FishId -> Game -> Random Game
act tankId fishId game =
    Maybe.map2
        (\currentLocation goal ->
            case goal of
                Idle ->
                    case
                        if game.fed |> Set.member fishId |> not then
                            tryEating tankId fishId game

                        else
                            Nothing
                    of
                        Just a ->
                            Random.constant a

                        Nothing ->
                            startMoving tankId fishId game
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
                                    |> Tuple.pair vector
                           )
                        |> (\( vector, goals ) ->
                                game.fish
                                    |> Dict.get fishId
                                    |> Maybe.map .size
                                    |> Maybe.withDefault 1
                                    |> (\size ->
                                            { game | goals = goals }
                                                |> moveTo
                                                    tankId
                                                    (let
                                                        vSize =
                                                            1 / toFloat size
                                                     in
                                                     if Vector.length vector < vSize then
                                                        location

                                                     else
                                                        vector
                                                            |> Vector.normalize
                                                            |> Vector.scaleBy vSize
                                                            |> Vector.addTo currentLocation
                                                    )
                                                    fishId
                                       )
                           )
                        |> Random.constant
        )
        (game.tanks
            |> Dict.get tankId
            |> Maybe.andThen (Tank.getFishLocation fishId)
        )
        (Dict.get fishId game.goals)
        |> Maybe.withDefault (Random.constant game)


moveTo : TankId -> ( Float, Float ) -> FishId -> Game -> Game
moveTo tankId location fishId game =
    game.tanks
        |> Dict.get tankId
        |> Maybe.andThen (Tank.getFishLocation fishId)
        |> Maybe.map
            (\oldLocation ->
                { game
                    | directions =
                        game.directions
                            |> Dict.insert fishId
                                (oldLocation
                                    |> Vector.to location
                                    |> Vector.angle
                                )
                    , tanks =
                        game.tanks
                            |> Dict.update tankId
                                (Maybe.map (Tank.moveFishTo location fishId))
                }
            )
        |> Maybe.withDefault game


mate : TankId -> ( FishId, FishId ) -> Game -> Random ( Game, List Action )
mate tankId ( fishId1, fishId2 ) game =
    let
        fishBreed1 =
            game.assignedBreed |> Dict.get fishId1

        fishBreed2 =
            game.assignedBreed |> Dict.get fishId2
    in
    Maybe.map3
        (\location fish1 fish2 ->
            Fish.Common.fromParents fish1 fish2
                |> Random.andThen
                    (\newFish ->
                        let
                            breed =
                                { pattern = newFish.pattern
                                , primary = newFish.primary
                                , secondary = newFish.secondary
                                }

                            newBreedId =
                                Fish.Common.getBreedId breed

                            breedId =
                                case ( fishBreed1, fishBreed2 ) of
                                    ( Just a, _ ) ->
                                        if a == newBreedId then
                                            Just newBreedId

                                        else
                                            Nothing

                                    ( _, Just a ) ->
                                        if a == newBreedId then
                                            Just newBreedId

                                        else
                                            Nothing

                                    ( Nothing, Nothing ) ->
                                        Nothing
                        in
                        { game
                            | fed =
                                game.fed
                                    |> Set.remove fishId1
                                    |> Set.remove fishId2
                        }
                            |> addFish location tankId newFish
                            |> (\( g, newFishId ) ->
                                    case breedId of
                                        Just b ->
                                            ( g |> assignBreedOf newFishId b
                                            , []
                                            )
                                                |> Random.constant

                                        Nothing ->
                                            if fish1.pattern == newFish.pattern then
                                                g
                                                    |> addBreed breed
                                                    |> Random.map
                                                        (\g2 ->
                                                            ( g2
                                                                |> assignBreedOf fishId1 newBreedId
                                                                |> assignBreedOf newFishId newBreedId
                                                            , [ Action.NewBreed newBreedId ]
                                                            )
                                                        )

                                            else if fish2.pattern == newFish.pattern then
                                                g
                                                    |> addBreed
                                                        { pattern = newFish.pattern
                                                        , primary = newFish.primary
                                                        , secondary = newFish.secondary
                                                        }
                                                    |> Random.map
                                                        (\g2 ->
                                                            ( g2
                                                                |> assignBreedOf fishId2 newBreedId
                                                                |> assignBreedOf newFishId newBreedId
                                                            , [ Action.NewBreed newBreedId ]
                                                            )
                                                        )

                                            else
                                                ( g, [] ) |> Random.constant
                               )
                    )
        )
        (game.tanks |> Dict.get tankId |> Maybe.andThen (Tank.getFishLocation fishId1))
        (game.fish |> Dict.get fishId1)
        (game.fish |> Dict.get fishId2)
        |> Maybe.withDefault (Random.constant ( game, [] ))


removeFromTank : TankId -> FishId -> Game -> Game
removeFromTank tankId fishId game =
    game.tanks
        |> Dict.update tankId
            (Maybe.map
                (Tank.removeFish fishId)
            )
        |> (\tanks -> { game | tanks = tanks })


insertIntoTank : ( Float, Float ) -> TankId -> FishId -> Game -> Game
insertIntoTank location tankId fishId game =
    { game
        | directions = game.directions |> Dict.insert fishId 0
        , goals = game.goals |> Dict.insert fishId Idle
        , tanks =
            game.tanks
                |> Dict.update tankId
                    (Maybe.map
                        (Tank.insertFish location fishId)
                    )
    }


randomOrder : Generator Order
randomOrder =
    let
        randBool =
            Random.int 0 1
                |> Random.map ((==) 1)

        randPigment =
            Random.map3
                (\red yellow blue ->
                    { red = red
                    , yellow = yellow
                    , blue = blue
                    }
                )
                randBool
                randBool
                randBool

        randCat =
            Random.map3
                (\primary secondary pattern ->
                    { primary = primary, secondary = secondary, pattern = pattern }
                )
                randPigment
                randPigment
                (Random.constant Cat.default.pattern)
    in
    Random.map3
        (\primary secondary cat ->
            { primary = primary, secondary = secondary, cat = cat }
        )
        randPigment
        randPigment
        randCat


price : Fish -> { primary : Pigment, secondary : Pigment } -> Int
price fish order =
    [ Config.basePrice
    , if fish.primary == order.primary then
        Config.featurePrice

      else
        0
    , if fish.secondary == order.secondary then
        Config.featurePrice

      else
        0
    ]
        |> List.sum


sellFish : FishId -> Game -> Generator Game
sellFish fishId game =
    if game.storage |> Set.member fishId then
        game.fish
            |> Dict.get fishId
            |> Maybe.map
                (\fish ->
                    randomOrder
                        |> Random.map
                            (\newOrder ->
                                { game
                                    | storage = game.storage |> Set.remove fishId
                                    , fish = game.fish |> Dict.remove fishId
                                    , money =
                                        game.money
                                            + price fish
                                                { primary = game.order.primary
                                                , secondary = game.order.secondary
                                                }
                                    , order = newOrder
                                }
                            )
                )
            |> Maybe.withDefault (Random.constant game)

    else
        Random.constant game


buyFish : Maybe Breed -> Game -> Generator Game
buyFish maybeBreed game =
    if game.money >= Config.fishCost then
        (case maybeBreed of
            Nothing ->
                Fish.generateDefault

            Just breed ->
                Fish.Common.new breed |> Random.constant
        )
            |> Random.map
                (\fish ->
                    { game
                        | fish = game.fish |> Dict.insert game.nextFishId fish
                        , nextFishId = game.nextFishId + 1
                        , money = game.money - Config.fishCost
                        , storage = game.storage |> Set.insert game.nextFishId
                    }
                )

    else
        Random.constant game


buyTank : Game -> Game
buyTank game =
    { game
        | tanks = game.tanks |> Dict.insert (Dict.size game.tanks) Tank.empty
        , money = game.money - Config.tankCost
    }


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


addFood : TankId -> Game -> Random Game
addFood tankId game =
    randomLocation
        |> Random.map
            (\location ->
                game.tanks
                    |> Dict.update tankId
                        (Maybe.map (Tank.insertFood location game.nextFoodId))
            )
        |> Random.map
            (\tanks ->
                { game
                    | tanks = tanks
                    , nextFoodId = game.nextFoodId + 1
                }
            )


addBreed :
    { pattern : Set ( Int, Int )
    , primary : Pigment
    , secondary : Pigment
    }
    -> Game
    -> Generator Game
addBreed args game =
    let
        randBreed name =
            { name = name
            , pattern = args.pattern
            , primary = args.primary
            , secondary = args.secondary
            }
    in
    (case game.breeds |> Dict.get (Fish.Common.getBreedId args) of
        Just { name } ->
            Random.constant name

        Nothing ->
            Fish.Name.random
    )
        |> Random.map randBreed
        |> Random.map
            (\breed ->
                { game
                    | breeds =
                        game.breeds
                            |> Dict.insert (Fish.Common.getBreedId breed) breed
                }
            )


assignBreedOf : FishId -> BreedId -> Game -> Game
assignBreedOf fishId breedId game =
    { game
        | assignedBreed =
            game.assignedBreed
                |> Dict.insert fishId breedId
    }
