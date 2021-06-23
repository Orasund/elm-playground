module GreenFields.Data.Effect exposing (Effect(..), toString)


type Effect
    = IncreaseInventorySpace Int


toString : Effect -> String
toString effect =
    case effect of
        IncreaseInventorySpace int ->
            "Increase the inventory space to " ++ String.fromInt int
