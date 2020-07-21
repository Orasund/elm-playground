module Ecocards.Data.GameArea exposing (GameArea, removeSet, add, draw, remove, tap)

import Array exposing (Array)
import Dict exposing (Dict)
import Ecocards.Data.Animal exposing (Animal)
import Set exposing (Set)


type alias GameArea =
    { deck : List Animal
    , hand : Array Animal
    , placed : Dict Int { isTapped : Bool }
    }


add : Animal -> GameArea -> GameArea
add animal gameArea =
    { gameArea | deck = animal :: gameArea.deck }


draw : GameArea -> GameArea
draw gameArea =
    case gameArea.deck of
        animal :: deck ->
            { deck = deck
            , hand = gameArea.hand |> Array.push animal
            , placed = gameArea.placed
            }

        [] ->
            gameArea


remove : Int -> GameArea -> GameArea
remove id gameArea =
    { gameArea | placed = gameArea.placed |> Dict.remove id }


removeSet : Set Int -> GameArea -> GameArea
removeSet set gameArea =
    set |> Set.foldl remove gameArea


tap : Int -> GameArea -> GameArea
tap id gameArea =
    { gameArea
        | placed = gameArea.placed |> Dict.update id (Maybe.map (always { isTapped = True }))
    }
