module Data.Info exposing (..)

import Bag
import Data.Block exposing (Block)
import Data.Game exposing (Game)
import Data.Item exposing (Item)


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


fromBlock : Game -> Block -> Info
fromBlock game block =
    case block of
        Data.Block.Ground maybeItem ->
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

        Data.Block.Vein item ->
            Data.Item.toString item ++ " Vein" |> fromTitle

        Data.Block.Wall ->
            fromTitle "Wall"

        Data.Block.Train ->
            fromTitle "Train"
                |> withContent
                    ([ String.fromInt game.train.coal ++ "x Coal"
                     , String.fromInt game.train.tracks ++ "x Tracks"
                     ]
                        ++ (game.train.items.bag
                                |> Bag.toAssociationList
                                |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                           )
                    )
                |> withAdditionalInfo
                    [ "Needs " ++ String.fromInt game.train.coalNeeded ++ " Coal to go back to HQ"
                    ]

        Data.Block.Track ->
            fromTitle "Track"

        Data.Block.Wagon list ->
            fromTitle "Wheelbarrow"
                |> withContent (list |> List.map Data.Item.toString)
