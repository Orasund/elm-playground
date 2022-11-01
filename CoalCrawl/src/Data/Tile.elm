module Data.Tile exposing (..)

import Data.Block exposing (Block)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Player exposing (Player)


type alias Tile =
    { color : String
    , content : Char
    , bold : Bool
    , big : Bool
    }


new : { color : String, content : Char } -> Tile
new args =
    { color = args.color
    , content = args.content
    , bold = False
    , big = False
    }


withBold : Tile -> Tile
withBold tile =
    { tile | bold = True }


withBigFont : Tile -> Tile
withBigFont tile =
    { tile | big = True }


wall : Tile
wall =
    { color = "Black", content = '█' } |> new


fromItem : Item -> Char
fromItem item =
    case item of
        Data.Item.Coal ->
            'c'

        Data.Item.IronOre ->
            'i'


fromPlayer : Player -> Tile
fromPlayer player =
    { color = "Green"
    , content =
        player.item
            |> Maybe.map fromItem
            |> Maybe.withDefault '@'
    }
        |> new
        |> withBold


fromFloor : Game -> Floor -> Tile
fromFloor game floor =
    case floor of
        Data.Floor.Ground maybeItem ->
            { color = "Gray"
            , content =
                maybeItem
                    |> Maybe.map fromItem
                    |> Maybe.withDefault '.'
            }
                |> new

        Data.Floor.Track ->
            { color = "Gray", content = '+' } |> new

        Data.Floor.Train ->
            { color = "Black", content = 'T' }
                |> new
                |> (if game.train.moving then
                        withBold

                    else
                        identity
                   )


fromEntity : Entity -> Tile
fromEntity entity =
    case entity of
        Data.Entity.Vein item ->
            { color = "Black", content = item |> fromItem |> Char.toUpper } |> new

        Data.Entity.Wall ->
            { color = "Black", content = '#' } |> new

        Data.Entity.RailwayTrack ->
            { color = "Black", content = '=' } |> new

        Data.Entity.Wagon _ ->
            { color = "Gray", content = 'W' } |> new


fromBlock : Game -> Block -> Tile
fromBlock game block =
    case block of
        Data.Block.FloorBlock floor ->
            fromFloor game floor

        Data.Block.EntityBlock entity ->
            fromEntity entity
