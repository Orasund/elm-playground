module Data.Block exposing (..)

import Config
import Data.Actor exposing (Actor)
import Data.Bomb
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Item exposing (Item)
import Data.Minecart


type Block
    = FloorBlock Floor
    | EntityBlock Entity
    | ActorBlock ( Int, Actor )


buildable : List Block
buildable =
    [ FloorBlock Data.Floor.Track
    , EntityBlock Data.Entity.container
    , ActorBlock ( -1, Data.Actor.Minecart Data.Minecart.emptyWagon )
    , ActorBlock ( -1, Data.Actor.Bomb Data.Bomb.new )
    ]


cost : Block -> Maybe ( Item, Int )
cost block =
    case block of
        FloorBlock Data.Floor.Track ->
            Just ( Data.Item.Iron, Config.trackCost )

        EntityBlock (Data.Entity.Container _) ->
            Just ( Data.Item.Iron, Config.containerCost )

        ActorBlock ( _, Data.Actor.Minecart _ ) ->
            Just ( Data.Item.Iron, Config.wagonCost )

        ActorBlock ( _, Data.Actor.Bomb _ ) ->
            Just ( Data.Item.Gold, Config.bombCost )

        _ ->
            Nothing
