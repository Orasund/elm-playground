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
    , content : List String
    , additionalInfo : List String
    }


fromTitle : String -> Info
fromTitle title =
    { title = title
    , content = []
    , additionalInfo = []
    }


withContent : List String -> Info -> Info
withContent content info =
    { info | content = content }


withAdditionalInfo : List String -> Info -> Info
withAdditionalInfo additionalInfos info =
    { info | additionalInfo = additionalInfos }


fromFloor : Game -> Floor -> Info
fromFloor game floor =
    case floor of
        Data.Floor.Ground maybeItem ->
            fromTitle "Ground"
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
            fromTitle "Track"

        Data.Floor.Train ->
            fromTitle "Train"
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


fromEntity : Entity -> Info
fromEntity entity =
    case entity of
        Data.Entity.Vein item ->
            Data.Item.toString item ++ " Vein" |> fromTitle

        Data.Entity.Wall { unstable } ->
            if unstable then
                fromTitle "Rotten Wall"

            else
                fromTitle "Wall"

        Data.Entity.RailwayTrack ->
            fromTitle "Railway Track"

        Data.Entity.Wagon wagon ->
            fromTitle "Wagon"
                |> withContent
                    (wagon.items
                        |> AnyBag.toAssociationList
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    )
                |> withAdditionalInfo
                    [ "Can store up to "
                        ++ String.fromInt Config.wagonMaxItems
                        ++ " items. You can also push it along."
                    ]

        Data.Entity.Water ->
            fromTitle "Water"


fromBlock : Game -> Block -> Info
fromBlock game block =
    case block of
        Data.Block.FloorBlock floor ->
            fromFloor game floor

        Data.Block.EntityBlock entity ->
            fromEntity entity
