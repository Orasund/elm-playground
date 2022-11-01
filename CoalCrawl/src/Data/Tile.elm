module Data.Tile exposing (..)

import Data.Block exposing (Block(..))
import Data.Info
import Data.Item exposing (Item)
import Data.Player exposing (Player)


type alias Tile =
    { color : String
    , content : Char
    , bold : Bool
    }


new : { color : String, content : Char } -> Tile
new args =
    { color = args.color
    , content = args.content
    , bold = False
    }


withBold : Tile -> Tile
withBold tile =
    { tile | bold = True }


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


fromWheelbarrow =
    { color = "Gray", content = 'W' } |> new


fromBlock : Block -> Tile
fromBlock block =
    case block of
        Ground maybeItem ->
            { color = "Gray"
            , content =
                maybeItem
                    |> Maybe.map fromItem
                    |> Maybe.withDefault '.'
            }
                |> new

        Vein item ->
            { color = "Black", content = item |> fromItem |> Char.toUpper } |> new

        Wall ->
            { color = "Black", content = '#' } |> new

        Train ->
            { color = "Black", content = 'T' } |> new

        Track ->
            { color = "Black", content = '=' } |> new

        Wagon _ ->
            fromWheelbarrow
