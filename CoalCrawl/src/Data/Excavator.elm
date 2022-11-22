module Data.Excavator exposing (..)

import AnyBag exposing (AnyBag)
import Config
import Data.Item exposing (Item)


type alias Excavator =
    { momentum : Maybe ( Int, Int )
    , items : AnyBag String Item
    , hasReversed : Bool
    }


new : Excavator
new =
    { momentum = Nothing
    , items = AnyBag.empty Data.Item.toString
    , hasReversed = False
    }


insertItem : Item -> Excavator -> Maybe Excavator
insertItem item excavator =
    if isFull excavator then
        Nothing

    else
        { excavator
            | items =
                excavator.items
                    |> AnyBag.insert 1 item
        }
            |> Just


isFull : Excavator -> Bool
isFull excavator =
    AnyBag.size excavator.items == Config.excavatorMaxItems


load : AnyBag String Item -> Excavator -> Excavator
load items excavator =
    { excavator | items = items }


unload : Excavator -> ( Excavator, AnyBag String Item )
unload excavator =
    ( { excavator | items = AnyBag.empty Data.Item.toString }
    , excavator.items
    )


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
