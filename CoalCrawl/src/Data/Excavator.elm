module Data.Excavator exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Storage exposing (Storage)


type alias Excavator =
    { storage : Storage
    , hasReversed : Bool
    }


new : Excavator
new =
    { storage = Data.Storage.empty Config.excavatorMaxItems
    , hasReversed = False
    }


insertItem : Item -> Excavator -> Maybe Excavator
insertItem item excavator =
    excavator.storage
        |> Data.Storage.insert item
        |> Maybe.map (\storage -> { excavator | storage = storage })
