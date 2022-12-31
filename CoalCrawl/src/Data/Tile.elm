module Data.Tile exposing (..)

import Config
import Data.Actor exposing (Actor(..))
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


image : { color : String, source : String } -> Tile
image args =
    { source = args.source
    , animation = False
    , color = args.color
    }
        |> ImageTile


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


fromPlayer : Game -> Player -> List Tile
fromPlayer game player =
    [ game.world
        |> Data.World.getFloor player.pos
        |> Maybe.withDefault Data.Floor.Ground
        |> fromFloor player.pos game
    , player.item
        |> Maybe.map Data.Item.toString
        |> Maybe.map (\item -> "assets/svg/player_" ++ String.toLower item ++ ".svg")
        |> Maybe.withDefault "assets/svg/player.svg"
        |> (\source ->
                { color = View.Color.green
                , source = source
                }
                    |> image
                    |> withAnimation
           )
    ]


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
                                    case track of
                                        Data.Floor.Track ->
                                            "1"

                                        Data.Floor.RailwayTrack ->
                                            "1"

                                        _ ->
                                            "0"
                                )
                            |> Maybe.withDefault "0"
                    )
                |> (\list ->
                        case list of
                            [ down, left, up, right ] ->
                                { color = View.Color.gray
                                , source = "assets/svg/track/track_" ++ down ++ left ++ up ++ right ++ ".svg"
                                }
                                    |> image

                            _ ->
                                { color = View.Color.gray
                                , source = "assets/svg/track/track_1111.svg"
                                }
                                    |> image
                   )

        Data.Floor.RailwayTrack ->
            { color = View.Color.gray
            , source = "assets/svg/railwayTrack.svg"
            }
                |> image


fromEntity : Entity -> Tile
fromEntity entity =
    case entity of
        Data.Entity.Vein item ->
            { source =
                "assets/svg/vein_"
                    ++ (item |> Data.Item.toString |> String.toLower)
                    ++ ".svg"
            , color = View.Color.black
            }
                |> image

        Data.Entity.Wall ->
            { source = "assets/svg/wall.svg"
            , color = View.Color.black
            }
                |> image

        Data.Entity.CrackedWall ->
            { source = "assets/svg/cracked_wall.svg"
            , color = View.Color.black
            }
                |> image

        Data.Entity.Water ->
            { color = View.Color.blue
            , source = "assets/svg/water.svg"
            }
                |> image

        Data.Entity.Lava ->
            { color = View.Color.red
            , source = "assets/svg/lava.svg"
            }
                |> image

        Data.Entity.Container storage ->
            { color = View.Color.black
            , source =
                if Data.Storage.isEmpty storage then
                    "assets/svg/container_empty.svg"

                else if Data.Storage.isFull storage then
                    "assets/svg/container_full.svg"

                else
                    "assets/svg/container.svg"
            }
                |> image
                |> (if Data.Storage.isFull storage then
                        withAnimation

                    else
                        identity
                   )


fromActor : Actor -> Tile
fromActor actor =
    case actor of
        Data.Actor.Minecart wagon ->
            (if Data.Storage.isEmpty wagon.storage then
                "assets/svg/minecart_empty.svg"

             else if Data.Storage.isFull wagon.storage then
                "assets/svg/minecart_full.svg"

             else
                "assets/svg/minecart.svg"
            )
                |> (\source -> image { source = source, color = View.Color.black })
                |> (\it ->
                        if Data.Storage.isFull wagon.storage then
                            it |> withAnimation

                        else
                            it
                   )

        Data.Actor.Helper _ ->
            { color = View.Color.red, content = '?' }
                |> new

        Data.Actor.Bomb bomb ->
            (if
                (bomb.explodesIn > Config.bombExplosionTime // 2)
                    || (bomb.explodesIn < Config.bombExplosionTime && modBy 2 bomb.explodesIn == 0)
             then
                "assets/svg/bomb_active.svg"

             else
                "assets/svg/bomb.svg"
            )
                |> (\source -> image { source = source, color = View.Color.red })

        Data.Actor.Train train ->
            "assets/svg/train.svg"
                |> (\source -> image { source = source, color = View.Color.black })
                |> (if train.moving || train.tracks > 0 then
                        withAnimation

                    else
                        identity
                   )

        Data.Actor.MovingWater _ ->
            { color = View.Color.blue
            , source = "assets/svg/water.svg"
            }
                |> image
                |> withAnimation


fromItem : Item -> Tile
fromItem item =
    "assets/svg/item_"
        ++ (item |> Data.Item.toString |> String.toLower)
        ++ ".svg"
        |> (\source ->
                { color = View.Color.gray
                , source = source
                }
                    |> image
           )


fromPos : ( Int, Int ) -> Game -> List Tile
fromPos pos game =
    [ game.world
        |> Data.World.getFloor pos
        |> Maybe.map (fromFloor pos game)
    , game.world
        |> Data.World.getItem pos
        |> Maybe.map fromItem
    , game.world
        |> Data.World.getEntity pos
        |> Maybe.map fromEntity
    , game.world
        |> Data.World.getActorAt pos
        |> Maybe.map Tuple.second
        |> Maybe.map fromActor
    ]
        |> List.filterMap identity
