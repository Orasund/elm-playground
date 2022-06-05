module View.Natural exposing (..)

import Gen.Enum.Natural exposing (Natural(..))
import Html.Styled exposing (Html)


view : Natural -> String
view natural =
    case natural of
        Wood ->
            "\u{1FAB5}"

        Stone ->
            "\u{1FAA8}"

        Leaf ->
            "🌿"

        Fruit ->
            "🍎"
