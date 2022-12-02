module Generation exposing (..)

import Config
import Data.Actor exposing (CaveType(..))
import Data.Block
import Data.Entity
import Data.World exposing (World)
import Generation.Wall
import Random exposing (Generator)


mine : ( Int, Int ) -> World -> Generator World
mine ( x, y ) world =
    case world |> Data.World.get ( x, y ) of
        Just ( Data.Block.EntityBlock entity, _ ) ->
            (case entity of
                Data.Entity.Vein item ->
                    [ item ]
                        |> Just

                Data.Entity.Actor _ ->
                    Nothing

                _ ->
                    Just []
            )
                |> Maybe.map
                    (\items ->
                        world
                            |> Data.World.removeEntity ( x, y )
                            |> (case items of
                                    [] ->
                                        identity

                                    item :: _ ->
                                        Data.World.insertItemAt ( x, y ) item
                               )
                            |> generateContent ( x, y )
                                (Generation.Wall.wallGenerator ( x, y ))
                    )
                |> Maybe.withDefault (Random.constant world)

        _ ->
            Random.constant world


baseProbability : ( Int, Int ) -> List ( Float, ( Int, Int ) )
baseProbability ( x, y ) =
    [ ( if y > Config.tracksPerTrip then
            0.5

        else
            0
      , ( x, y - 1 )
      )
    , ( 0.5, ( x, y + 1 ) )
    , ( 0.5, ( x - 1, y ) )
    , ( 0.5, ( x + 1, y ) )
    ]


caveGenerator :
    { ground : Generator (( Int, Int ) -> World -> World)
    , cave : CaveType
    }
    -> ( Int, Int )
    -> World
    -> Generator World
caveGenerator args pos world =
    args.ground
        |> Random.andThen
            (\fun ->
                world
                    |> Data.World.removeEntity pos
                    |> fun pos
                    |> generateContent pos
                        (Random.weighted
                            ( 0
                            , Data.Entity.Wall
                                |> Data.World.insertEntity
                                |> Random.constant
                            )
                            [ ( 1
                              , args.cave
                                    |> Data.Actor.Cave
                                    |> Data.Actor.Helper
                                    |> Data.World.insertActor
                                    |> Random.constant
                              )
                            , ( 1 / 2, Generation.Wall.wallGenerator pos )
                            , ( 1 / 4
                              , Data.Actor.Path
                                    |> Data.Actor.Helper
                                    |> Data.World.insertActor
                                    |> Random.constant
                              )
                            , ( 1 / 16
                              , Data.Actor.Mine
                                    |> Data.Actor.Helper
                                    |> Data.World.insertActor
                                    |> Random.constant
                              )
                            ]
                            |> Random.andThen identity
                        )
            )


{-| base function for generating content
-}
generateContent : ( Int, Int ) -> Generator (( Int, Int ) -> World -> World) -> World -> Generator World
generateContent pos content dict =
    baseProbability pos
        |> List.foldl
            (\( prob, p ) ->
                Random.andThen
                    (\w ->
                        Random.float 0 1
                            |> Random.andThen
                                (\float ->
                                    generateAt p
                                        (if float < prob then
                                            content

                                         else
                                            Data.World.insertEntity Data.Entity.Wall
                                                |> Random.constant
                                        )
                                        w
                                )
                    )
            )
            (Random.constant dict)


generateAt : ( Int, Int ) -> Generator (( Int, Int ) -> World -> World) -> World -> Generator World
generateAt pos fun world =
    fun
        |> Random.map
            (\updateAt ->
                world
                    |> Data.World.get pos
                    |> (\maybe ->
                            if maybe == Nothing then
                                world
                                    |> updateAt pos

                            else
                                world
                       )
            )
