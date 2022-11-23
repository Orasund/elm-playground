module Data.Info exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Entity exposing (Entity(..))
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item
import Data.Storage
import Data.Train
import Dict


type alias Info =
    { title : String
    , description : String
    , content : List String
    , additionalInfo : List String
    }


new : { title : String, description : String } -> Info
new args =
    { title = args.title
    , description = args.description
    , content = []
    , additionalInfo = []
    }


withContent : List String -> Info -> Info
withContent content info =
    { info | content = content }


withAdditionalInfo : List String -> Info -> Info
withAdditionalInfo additionalInfos info =
    { info | additionalInfo = additionalInfos }


fromFloor : Floor -> Info
fromFloor floor =
    case floor of
        Data.Floor.Ground ->
            new
                { title = "Ground"
                , description = "May contain an item that can be picked up."
                }

        Data.Floor.Track ->
            new
                { title = "Track"
                , description = "Pushed wagons will automatically move to adjacent tracks"
                }

        Data.Floor.RailwayTrack ->
            new
                { title = "Railway Track"
                , description = "Trains will move along railway tracks"
                }


fromTrain : Game -> Info
fromTrain game =
    let
        train =
            game |> Data.Game.getTrain
    in
    new
        { title = "Train"
        , description = ""
        }
        |> withContent
            ((String.fromInt train.tracks ++ "x Tracks")
                :: (train.items
                        |> AnyBag.toAssociationList
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                   )
            )
        |> withAdditionalInfo
            [ "Needs " ++ String.fromInt (Data.Train.coalNeeded train) ++ " Coal to go back to HQ."
            ]


fromEntity : Game -> Entity -> Info
fromEntity game entity =
    case entity of
        Data.Entity.Vein item ->
            new
                { title = Data.Item.toString item ++ " Vein"
                , description = "Drops one " ++ Data.Item.toString item ++ " when mined."
                }

        Data.Entity.Wall ->
            new
                { title = "Wall"
                , description = "Can be mind by bombs, but will not drop anything."
                }

        Data.Entity.Water ->
            new
                { title = "Water"
                , description = "Will be pushed aside when you walk through it. Wagons can't pass through it."
                }

        Data.Entity.Lava ->
            new
                { title = "Lava"
                , description = "Gets removed if water gets moved into it."
                }

        Data.Entity.Actor id ->
            game.world.actors
                |> Dict.get id
                |> Maybe.map (\( _, actor ) -> fromActor actor)
                |> Maybe.withDefault
                    (new
                        { title = "Unkown Actor"
                        , description = "This is a bug. Please report how to managed to create this entity"
                        }
                    )


cave caveType =
    case caveType of
        Data.Actor.WaterCave ->
            "Water"

        Data.Actor.CoalCave ->
            "Coal"

        Data.Actor.IronCave ->
            "Iron"

        Data.Actor.LavaCave ->
            "Gold"


fromActor : Actor -> Info
fromActor actor =
    case actor of
        Data.Actor.Minecart wagon ->
            new
                { title = "Minecart"
                , description =
                    "Can store up to "
                        ++ String.fromInt Config.wagonMaxItems
                        ++ " items. You can also push it along."
                }
                |> withContent
                    (wagon.storage
                        |> Data.Storage.toList
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    )

        Data.Actor.Helper _ ->
            { title = "Helper"
            , description = "Helper Block used to generate content"
            }
                |> new

        Data.Actor.Bomb { explodesIn } ->
            { title = "Bomb"
            , description = "Explodes in " ++ String.fromInt explodesIn ++ " turns."
            }
                |> new

        Data.Actor.Excavator excavator ->
            { title = "Excavator"
            , description = "Mines and collect automatically"
            }
                |> new
                |> withContent
                    (excavator.storage
                        |> Data.Storage.toList
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    )

        Data.Actor.Train train ->
            { title = "Train"
            , description = "Stores all your items. If it has tracks stored, it will place them and move forward. Needs coal to move. Will regularly fetch new tracks from above ground."
            }
                |> new
                |> withContent
                    (train.items
                        |> AnyBag.toAssociationList
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    )


fromBlock : Game -> Block -> Info
fromBlock game block =
    case block of
        Data.Block.FloorBlock floor ->
            fromFloor floor

        Data.Block.EntityBlock entity ->
            fromEntity game entity
