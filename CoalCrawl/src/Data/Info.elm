module Data.Info exposing (..)

import AnyBag
import Config
import Data.Block exposing (Block)
import Data.Entity exposing (Entity(..))
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item


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
        Data.Floor.Ground maybeItem ->
            new
                { title = "Ground"
                , description = "May contain an item that can be picked up."
                }
                |> withContent
                    (maybeItem
                        |> Maybe.map
                            (\item ->
                                item
                                    |> Data.Item.toString
                                    |> List.singleton
                            )
                        |> Maybe.withDefault []
                    )

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


fromEntity : Game -> Entity -> Info
fromEntity game entity =
    case entity of
        Data.Entity.Vein item ->
            new
                { title = Data.Item.toString item ++ " Vein"
                , description = "Drops one " ++ Data.Item.toString item ++ "when mined"
                }

        Data.Entity.Wall ->
            new
                { title = "Wall"
                , description = "Can be mind by bombs, but will not drop anything."
                }

        Data.Entity.Cave caveType ->
            { title =
                (case caveType of
                    Data.Entity.WaterCave ->
                        "Water"

                    Data.Entity.RubbleCave ->
                        "Rubble"
                )
                    ++ " Cave"
            , description = "Helper Block to generate caves"
            }
                |> new

        Data.Entity.Wagon wagon ->
            new
                { title = "Wagon"
                , description =
                    "Can store up to "
                        ++ String.fromInt Config.wagonMaxItems
                        ++ " items. You can also push it along."
                }
                |> withContent
                    (wagon.items
                        |> AnyBag.toAssociationList
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    )

        Data.Entity.Water ->
            new
                { title = "Water"
                , description = "Will be pushed aside when you walk through it. Wagons can't pass through it."
                }

        Data.Entity.Train ->
            new
                { title = "Train"
                , description = "Stores all your items. If it has tracks stored, it will place them and move forward. Needs coal to move. Will regularly fetch new tracks from above ground."
                }
                |> withContent
                    ((String.fromInt game.train.tracks ++ "x Tracks")
                        :: (game.train.items
                                |> AnyBag.toAssociationList
                                |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                           )
                    )
                |> withAdditionalInfo
                    [ "Needs " ++ String.fromInt game.train.coalNeeded ++ " Coal to go back to HQ"
                    ]

        Data.Entity.Rubble anyBag ->
            new
                { title = "Rubble"
                , description = "Contains multiple items. Items can be picked up. Will be removed once it does not contain any items."
                }
                |> withContent
                    (anyBag
                        |> AnyBag.fromList Data.Item.toString
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
