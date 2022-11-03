module Data.Tile exposing (..)

import AnyBag
import Config
import Data.Block exposing (Block)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Player exposing (Player)
import View.Color


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
    { color = View.Color.black, content = ' ' } |> new


fromItem : Item -> Char
fromItem item =
    case item of
        Data.Item.Coal ->
            'c'

        Data.Item.Iron ->
            'i'

        Data.Item.Gold ->
            'g'


fromPlayer : Player -> Tile
fromPlayer player =
    { color = View.Color.green
    , content =
        player.item
            |> Maybe.map fromItem
            |> Maybe.withDefault '@'
    }
        |> new
        |> withBold


fromFloor : Floor -> Tile
fromFloor floor =
    case floor of
        Data.Floor.Ground maybeItem ->
            { color = View.Color.gray
            , content =
                maybeItem
                    |> Maybe.map fromItem
                    |> Maybe.withDefault '.'
            }
                |> new

        Data.Floor.Track ->
            { color = View.Color.gray, content = '+' } |> new

        Data.Floor.RailwayTrack ->
            { color = View.Color.black, content = '=' } |> new


fromEntity : Game -> Entity -> Tile
fromEntity game entity =
    case entity of
        Data.Entity.Vein item ->
            { color = View.Color.black, content = item |> fromItem |> Char.toUpper } |> new

        Data.Entity.Wall { unstable } ->
            if unstable then
                { color = View.Color.black, content = '%' } |> new

            else
                { color = View.Color.black, content = '#' } |> new

        Data.Entity.Wagon wagon ->
            { color =
                if AnyBag.size wagon.items == Config.wagonMaxItems then
                    View.Color.black

                else
                    View.Color.gray
            , content = 'W'
            }
                |> new

        Data.Entity.Water ->
            { color = View.Color.blue, content = '~' } |> new

        Data.Entity.Train ->
            { color = View.Color.black, content = 'T' }
                |> new
                |> (if game.train.moving || game.train.tracks > 0 then
                        withBold

                    else
                        identity
                   )


fromBlock : Game -> Block -> Tile
fromBlock game block =
    case block of
        Data.Block.FloorBlock floor ->
            fromFloor floor

        Data.Block.EntityBlock entity ->
            fromEntity game entity
