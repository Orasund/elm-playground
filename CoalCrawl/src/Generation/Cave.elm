module Generation.Cave exposing (..)

import Data.Actor exposing (CaveType(..))
import Data.Entity
import Data.Item
import Data.World exposing (World)
import Generation exposing (caveGenerator)
import Random exposing (Generator)


exposedCave : CaveType -> ( Int, Int ) -> World -> Generator World
exposedCave caveType =
    (case caveType of
        CoalCave ->
            Random.weighted ( 1, Data.World.insertItem Data.Item.Coal )
                [ ( 1 / 2
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Coal)
                            |> Data.Actor.Helper
                        )
                  )
                ]

        IronCave ->
            Random.weighted
                ( 1
                , Data.World.insertItem Data.Item.Iron
                )
                [ ( 1 / 2
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Coal)
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 4
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Iron)
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 8, Data.World.insertEntity Data.Entity.Wall )
                ]

        WaterCave ->
            Random.weighted ( 1, Data.World.insertItem Data.Item.Gold )
                [ ( 1 / 2
                  , Data.World.insertActor
                        (Data.Actor.Falling Data.Entity.Water
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 4, Data.World.insertItem Data.Item.Coal )
                ]

        LavaCave ->
            Random.weighted ( 1, Data.World.insertEntity Data.Entity.Lava )
                [ ( 1 / 2, Data.World.insertItem Data.Item.Coal )
                , ( 1 / 4, Data.World.insertItem Data.Item.Gold )
                ]

        CollapsedCave ->
            Random.weighted
                ( 1
                , Data.World.insertActor (Data.Actor.Helper Data.Actor.Path)
                )
                [ ( 1 / 2
                  , Data.World.insertActor
                        (Data.Actor.Falling Data.Entity.Wall
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 4
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Coal)
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 8
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Gold)
                            |> Data.Actor.Helper
                        )
                  )
                ]
    )
        |> (\ground ->
                caveGenerator
                    { ground = ground
                    , cave = caveType
                    }
           )
