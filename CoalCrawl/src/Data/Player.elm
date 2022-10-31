module Data.Player exposing (..)

import Data.Item exposing (Item)


type alias Player =
    { pos : ( Int, Int )
    , item : Maybe Item
    }


fromPos : ( Int, Int ) -> Player
fromPos pos =
    { pos = pos, item = Nothing }


moveTo : ( Int, Int ) -> Player -> Player
moveTo pos player =
    { player | pos = pos }


hold : Item -> Player -> Maybe Player
hold item player =
    case player.item of
        Just _ ->
            Nothing

        Nothing ->
            Just { player | item = Just item }


dropItem : Player -> Maybe ( Player, Item )
dropItem player =
    player.item
        |> Maybe.map (\i -> ( { player | item = Nothing }, i ))
