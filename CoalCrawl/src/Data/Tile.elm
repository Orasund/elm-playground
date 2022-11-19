module Data.Tile exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor(..))
import Data.Block exposing (Block)
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Player exposing (Player)
import Dict
import View.Color


type alias Tile =
    { color : String
    , content : Char
    , bold : Bool
    , animation : Bool
    , size : Float
    }


new : { color : String, content : Char } -> Tile
new args =
    { color = args.color
    , content = args.content
    , bold = False
    , animation = False
    , size = 1
    }


emoji : Char -> Tile
emoji char =
    { color = View.Color.black
    , content = char
    , bold = False
    , animation = False
    , size = 0.7
    }


withSmall : Tile -> Tile
withSmall tile =
    { tile | size = 0.2 }


withBold : Tile -> Tile
withBold tile =
    { tile | bold = True }


withAnimation : Tile -> Tile
withAnimation tile =
    { tile | animation = True }


wall : Tile
wall =
    { color = View.Color.black, content = ' ' } |> new



fromPlayer : Player -> Tile
fromPlayer player =
    { color = View.Color.green
    , content =
        player.item
            |> Maybe.map Data.Item.toChar
            |> Maybe.withDefault '@'
    }
        |> new
        |> withBold


fromFloor : Floor -> Char
fromFloor floor =
    case floor of
        Data.Floor.Ground ->
            '.'

        Data.Floor.Track ->
            '+'

        Data.Floor.RailwayTrack ->
            '='


fromEntity : Game -> Entity -> Tile
fromEntity game entity =
    case entity of
        Data.Entity.Vein item ->
            item
                |> Data.Item.toChar
                |> Char.toUpper
                |> emoji

        Data.Entity.Wall ->
            { color = View.Color.black, content = '#' } |> new

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

        Data.Entity.Actor id ->
            game.world.actors
                |> Dict.get id
                |> Maybe.map (\( _, actor ) -> fromActor actor)
                |> Maybe.withDefault
                    (new { color = View.Color.red, content = '?' })


fromActor : Actor -> Tile
fromActor actor =
    case actor of
        Data.Actor.Wagon wagon ->
            { color =
                if AnyBag.size wagon.items == Config.wagonMaxItems then
                    View.Color.black

                else
                    View.Color.gray
            , content = 'W'
            }
                |> new
                |> (\it ->
                        if AnyBag.size wagon.items == 0 then
                            it

                        else if AnyBag.size wagon.items == Config.wagonMaxItems then
                            it |> withAnimation |> withBold

                        else
                            it |> withBold
                   )

        Data.Actor.Cave _ ->
            { color = View.Color.red, content = '#' }
                |> new
                |> withBold

        Data.Actor.Mine ->
            { color = View.Color.red, content = '#' }
                |> new
                |> withBold

        Data.Actor.FallingCoal ->
            { color = View.Color.red, content = 'C' }
                |> new
                |> withBold

        Data.Actor.Path ->
            { color = View.Color.red, content = '.' }
                |> new

        Data.Actor.Bomb bomb ->
            { color = View.Color.red, content = 'b' }
                |> new
                |> (if
                        (bomb.explodesIn > Config.bombExplosionTime // 2)
                            || (bomb.explodesIn < Config.bombExplosionTime && modBy 2 bomb.explodesIn == 0)
                    then
                        withBold

                    else
                        identity
                   )


fromBlock : Game -> ( Block, Maybe Item ) -> Tile
fromBlock game ( block, items ) =
    case block of
        Data.Block.FloorBlock floor ->
            (case items of
                Just item ->
                    item|> Data.Item.toChar 
                    |> emoji
                    |> withSmall

                Nothing ->
                    { color = View.Color.gray
                        , content = fromFloor floor}
                    |> new
            )
                

        Data.Block.EntityBlock entity ->
            fromEntity game entity
