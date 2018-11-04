module RuineJump.Tileset exposing
    ( dirt
    , grass
    , player_jump_left
    , player_jump_right
    , player_left
    , player_right
    , stone
    )

import Natural exposing (Natural16(..))
import PixelEngine.Graphics.Tile exposing (Tile, movable, tile)


variantTile : Natural16 -> ( Int, Int ) -> Tile msg
variantTile nat16 ( x, y ) =
    let
        seed : Int
        seed =
            nat16 |> Natural.toIntFrom16
    in
    tile ( x + (seed |> modBy 4), y + seed // 4 )


quadCoordinates : ( Int, Int ) -> List ( Int, Int )
quadCoordinates ( x, y ) =
    [ ( x, y ), ( x + 1, y ), ( x, y + 1 ), ( x + 1, y + 1 ) ]


quadTile : { tilePos : ( Int, Int ), pos : ( Int, Int ) } -> List ( ( Int, Int ), Tile msg )
quadTile { tilePos, pos } =
    List.map2
        (\p t -> ( p, t ))
        (quadCoordinates pos)
        (quadCoordinates tilePos |> List.map tile)


dirt : Natural16 -> Tile msg
dirt seed =
    variantTile seed ( 4, 0 )
        |> movable (String.fromInt <| Natural.toIntFrom16 seed)


stone : Natural16 -> Tile msg
stone seed =
    variantTile seed ( 0, 4 )


grass : Natural16 -> Tile msg
grass seed =
    variantTile seed ( 4, 4 )


player_left : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_left pos =
    quadTile { tilePos = ( 0, 8 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)


player_right : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_right pos =
    quadTile { tilePos = ( 2, 8 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)


player_jump_left : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_jump_left pos =
    quadTile { tilePos = ( 0, 10 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)


player_jump_right : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_jump_right pos =
    quadTile { tilePos = ( 2, 10 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)
