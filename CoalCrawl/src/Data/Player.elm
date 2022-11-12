module Data.Player exposing (..)

import Data.Block exposing (Block(..))
import Data.Item exposing (Item)


type alias Player =
    { pos : ( Int, Int )
    , targetPos : Maybe ( Int, Int )
    , item : Maybe Item
    , riding : Maybe Int
    }


fromPos : ( Int, Int ) -> Player
fromPos pos =
    { pos = pos
    , targetPos = Nothing
    , item = Nothing
    , riding = Nothing
    }


startRiding : Int -> Player -> Player
startRiding id player =
    { player | riding = Just id }


stopRiding : Player -> Player
stopRiding player =
    { player | riding = Nothing }


startMovingTo : ( Int, Int ) -> Player -> Player
startMovingTo pos player =
    { player | targetPos = Just pos }


stopMoving : Player -> Player
stopMoving player =
    { player | targetPos = Nothing }


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
