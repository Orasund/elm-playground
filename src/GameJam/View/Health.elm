module GameJam.View.Health exposing (view)

import GameJam.Data exposing (screenWidth, spriteSize)
import GameJam.Data.Square exposing (Square(..))
import GameJam.View as View
import GameJam.View.Square as Square
import Location exposing (Location)
import PixelEngine.Image as Image exposing (Image)


view : Int -> List ( Location, Image msg )
view amount =
    View.tileset
        |> Image.fromTile (Square.view Health)
        |> List.repeat amount
        |> List.indexedMap
            (\i image ->
                ( ( ((screenWidth - (toFloat <| amount * spriteSize)) / 2)
                        + (toFloat <| i * spriteSize)
                  , toFloat <| 0 * spriteSize
                  )
                , image
                )
            )
