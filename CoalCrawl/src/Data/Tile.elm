module Data.Tile exposing (..)

import Config
import Data.Actor exposing (Actor(..))
import Data.Block
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Player exposing (Player)
import Data.Position
import Data.Storage
import Data.World
import View.Color


type alias CharTileContent =
    { color : String
    , content : Char
    , bold : Bool
    , animation : Bool
    , size : Float
    }


type alias ImageTileContent =
    { source : String
    , animation : Bool
    , color : String
    }


type Tile
    = CharTile CharTileContent
    | ImageTile ImageTileContent


new : { color : String, content : Char } -> Tile
new args =
    { color = args.color
    , content = args.content
    , bold = False
    , animation = False
    , size = 1
    }
        |> CharTile


emoji : Char -> Tile
emoji char =
    { color = View.Color.black
    , content = char
    , bold = False
    , animation = False
    , size = 0.7
    }
        |> CharTile


image : { color : String, source : String } -> Tile
image args =
    { source = args.source
    , animation = False
    , color = args.color
    }
        |> ImageTile


withSmall : Tile -> Tile
withSmall tile =
    case tile of
        CharTile content ->
            { content | size = 0.2 } |> CharTile

        ImageTile _ ->
            tile


withBold : Tile -> Tile
withBold tile =
    case tile of
        CharTile content ->
            { content | bold = True } |> CharTile

        ImageTile _ ->
            tile


withAnimation : Tile -> Tile
withAnimation tile =
    case tile of
        CharTile content ->
            { content | animation = True } |> CharTile

        ImageTile content ->
            { content | animation = True } |> ImageTile


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


fromFloor : ( Int, Int ) -> Game -> Floor -> Tile
fromFloor pos game floor =
    case floor of
        Data.Floor.Ground ->
            { color = View.Color.gray
            , content = ' '
            }
                |> new

        Data.Floor.Track ->
            pos
                |> Data.Position.neighbors
                |> List.map
                    (\p ->
                        game.world
                            |> Data.World.getFloor p
                            |> Maybe.map
                                (\track ->
                                    if track == Data.Floor.Track then
                                        "1"

                                    else
                                        "0"
                                )
                            |> Maybe.withDefault "0"
                    )
                |> (\list ->
                        case list of
                            [ down, left, up, right ] ->
                                { color = View.Color.gray
                                , source = "/assets/svg/track_" ++ down ++ left ++ up ++ right ++ ".svg"
                                }
                                    |> image

                            _ ->
                                { color = View.Color.gray
                                , source = "/assets/svg/track_1111.svg"
                                }
                                    |> image
                   )

        Data.Floor.RailwayTrack ->
            { color = View.Color.gray
            , source = "/assets/svg/railwayTrack.svg"
            }
                |> image


fromEntity : Game -> Entity -> List Tile
fromEntity game entity =
    case entity of
        Data.Entity.Vein item ->
            item
                |> Data.Item.toChar
                |> Char.toUpper
                |> emoji
                |> List.singleton

        Data.Entity.Wall ->
            image { source = "/assets/svg/wall.svg", color = View.Color.black }
                |> List.singleton

        Data.Entity.Water ->
            { color = View.Color.blue, content = '~' }
                |> new
                |> List.singleton

        Data.Entity.Lava ->
            { color = View.Color.red, content = '~' }
                |> new
                |> List.singleton

        Data.Entity.Actor id ->
            game.world
                |> Data.World.getActor id
                |> Maybe.map
                    (\( pos, actor ) ->
                        [ game.world
                            |> Data.World.getFloor pos
                            |> Maybe.withDefault Data.Floor.Ground
                            |> fromFloor pos game
                        , fromActor actor
                        ]
                    )
                |> Maybe.withDefault
                    ({ color = View.Color.red, content = '?' } |> new |> List.singleton)


fromActor : Actor -> Tile
fromActor actor =
    case actor of
        Data.Actor.Minecart wagon ->
            (if Data.Storage.isEmpty wagon.storage then
                "/assets/svg/minecart.svg"

             else
                "/assets/svg/minecart_full.svg"
            )
                |> (\source -> image { source = source, color = View.Color.black })
                |> (\it ->
                        if Data.Storage.isFull wagon.storage then
                            it |> withAnimation |> withBold

                        else
                            it |> withBold
                   )

        Data.Actor.Excavator _ ->
            { color = View.Color.black
            , content = 'E'
            }
                |> new

        Data.Actor.Helper _ ->
            { color = View.Color.red, content = '?' }
                |> new
                |> withBold

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

        Data.Actor.Train train ->
            "/assets/svg/train.svg"
                |> (\source -> image { source = source, color = View.Color.black })
                |> (if train.moving || train.tracks > 0 then
                        withBold

                    else
                        identity
                   )

        Data.Actor.MovingWater _ ->
            { color = View.Color.blue
            , content = '~'
            }
                |> new
                |> withBold


fromItem : Item -> List Tile
fromItem item =
    item
        |> Data.Item.toChar
        |> emoji
        |> withSmall
        |> List.singleton


fromPos : ( Int, Int ) -> Game -> List Tile
fromPos pos game =
    game.world
        |> Data.World.get pos
        |> Maybe.map
            (\( block, items ) ->
                case block of
                    Data.Block.FloorBlock floor ->
                        fromFloor pos game floor
                            :: (items
                                    |> Maybe.map fromItem
                                    |> Maybe.withDefault []
                               )

                    Data.Block.EntityBlock entity ->
                        fromEntity game entity
            )
        |> Maybe.withDefault []
