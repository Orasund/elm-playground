module Ecocards.View.Biome exposing (asColor)

import Color exposing (Color)
import Ecocards.Data.Animal exposing (Biome(..))


asColor : Biome -> Color
asColor biome =
    case biome of
        --Fauna ->
        --    Color.rgb255 0 158 115
        Plain ->
            Color.rgb255 240 228 66

        River ->
            Color.rgb255 86 180 233
