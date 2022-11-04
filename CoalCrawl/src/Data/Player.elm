module Data.Player exposing (..)

import Data.Block exposing (Block(..))
import Data.Item exposing (Item)


type alias Player =
    { pos : ( Int, Int )
    , item : Maybe Item
    , riding : Maybe ( Int, Int )
    }


fromPos : ( Int, Int ) -> Player
fromPos pos =
    { pos = pos, item = Nothing, riding = Nothing }


startRiding : ( Int, Int ) -> Player -> Player
startRiding pos player =
    { player | riding = Just pos }


stopRiding : Player -> Player
stopRiding player =
    { player | riding = Nothing }


moveTo : ( Int, Int ) -> Player -> Player
moveTo pos player =
    { player
        | pos = pos
    }


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
