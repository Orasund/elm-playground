module DigDigBoom.Tileset exposing (dirt,stone,grass)

import Random
import PixelEngine.Graphics.Tile exposing (Tile,tile)

variantTile : (Int,Int) -> Random.Generator (Tile msg)
variantTile (x,y) = Random.pair (Random.int x (x+3)) (Random.int y (y+3))
  |> Random.map tile

dirt : Random.Generator (Tile msg)
dirt = variantTile (4,0)

stone : Random.Generator (Tile msg)
stone = variantTile (0,4)

grass : Random.Generator (Tile msg)
grass = variantTile (4,4)