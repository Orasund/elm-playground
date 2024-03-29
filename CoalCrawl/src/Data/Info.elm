module Data.Info exposing (..)

import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Entity exposing (Entity(..))
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item
import Data.Storage
import Data.Train


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
        |> withAdditionalInfo
            [ "Needs " ++ String.fromInt (Data.Train.coalNeeded train) ++ " Coal to go back to HQ."
            ]


fromEntity : Entity -> Info
fromEntity entity =
    case entity of
        Data.Entity.Vein item ->
            new
                { title = Data.Item.toString item ++ " Vein"
                , description = "Drops one " ++ Data.Item.toString item ++ " when mined."
                }

        Data.Entity.Wall ->
            new
                { title = "Wall"
                , description = "Indestructible."
                }

        Data.Entity.CrackedWall ->
            new
                { title = "Cracked Wall"
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

        Data.Entity.Container storage ->
            new
                { title = "Container"
                , description = "Can hold up to " ++ String.fromInt Config.containerMaxItems ++ " items"
                }
                |> withAdditionalInfo
                    [ "Contains " ++ (storage |> Data.Storage.size |> String.fromInt) ++ " items."
                    ]


fromActor : Actor -> Info
fromActor actor =
    case actor of
        Data.Actor.Minecart _ ->
            new
                { title = "Minecart"
                , description =
                    "Can store up to "
                        ++ String.fromInt Config.wagonMaxItems
                        ++ " items. You can also push it along."
                }

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

        Data.Actor.Train _ ->
            { title = "Train"
            , description = "Stores all your items. If it has tracks stored, it will place them and move forward. Needs coal to move. Will regularly fetch new tracks from above ground."
            }
                |> new

        Data.Actor.MovingWater _ ->
            { title = "Moving Water"
            , description = "Will stop moving as soon as possible"
            }
                |> new


fromBlock : Block -> Info
fromBlock block =
    case block of
        Data.Block.FloorBlock floor ->
            fromFloor floor

        Data.Block.EntityBlock entity ->
            fromEntity entity

        Data.Block.ActorBlock ( _, actor ) ->
            fromActor actor
