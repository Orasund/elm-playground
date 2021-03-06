module HeroForge.Data.Item exposing (Item(..), asSymbol, toString)

import HeroForge.Data.Level exposing (Level)


type Item
    = Mail Level
    | Quest


toString : Item -> String
toString item =
    case item of
        Mail level ->
            "Mail"

        Quest ->
            "Quest"


asSymbol : Item -> String
asSymbol item =
    case item of
        Mail level ->
            "✉️"

        Quest ->
            "👑"
