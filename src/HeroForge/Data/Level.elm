module HeroForge.Data.Level exposing (Level(..), toString)


type Level
    = Village
    | Forest
    | Mountains


toString : Level -> String
toString level =
    case level of
        Village ->
            "Village"

        Forest ->
            "Forest"

        Mountains ->
            "Mountains"
