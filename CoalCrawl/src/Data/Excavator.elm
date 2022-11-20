module Data.Excavator exposing (..)


type alias Excavator =
    { momentum : Maybe ( Int, Int )
    , hasReversed : Bool
    }


new : Excavator
new =
    { momentum = Nothing
    , hasReversed = False
    }


reverse : Excavator -> Excavator
reverse excavator =
    excavator.momentum
        |> Maybe.map
            (\( x, y ) ->
                { excavator
                    | momentum =
                        if excavator.hasReversed then
                            Nothing

                        else
                            Just ( -x, -y )
                    , hasReversed = not excavator.hasReversed
                }
            )
        |> Maybe.withDefault excavator
