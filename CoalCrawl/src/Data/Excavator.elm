module Data.Excavator exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Storage exposing (Storage)


type alias Excavator =
    { momentum : Maybe ( Int, Int )
    , storage : Storage
    , hasReversed : Bool
    }


new : Excavator
new =
    { momentum = Nothing
    , storage = Data.Storage.empty Config.excavatorMaxItems
    , hasReversed = False
    }


insertItem : Item -> Excavator -> Maybe Excavator
insertItem item excavator =
    excavator.storage
        |> Data.Storage.insert item
        |> Maybe.map (\storage -> { excavator | storage = storage })


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
