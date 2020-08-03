module Ecocards.Data.GameArea exposing (GameArea, add, draw, endTurn, generate, remove, removeSet, tap)

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Ecocards.Data.Animal exposing (Animal)
import List.Extra as List
import Random exposing (Generator)
import Random.List as Random
import Set exposing (Set)


type alias GameArea =
    { deck : List Animal
    , hand : Array Animal
    , placed : Dict Int { isTapped : Bool }
    }


generate : List Animal -> Generator GameArea
generate =
    Random.shuffle
        >> Random.map
            (\deck ->
                { deck = deck
                , hand = Array.empty
                , placed = Dict.empty
                }
                    |> draw
                    |> draw
                    |> draw
            )


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


endTurn : GameArea -> GameArea
endTurn gameArea =
    let
        ( freshDraw, deck ) =
            gameArea.deck
                |> List.splitAt (3 - (gameArea.hand |> Array.length))
    in
    { gameArea
        | placed =
            gameArea.placed
                |> Dict.filterMap
                    (\_ { isTapped } ->
                        if isTapped then
                            Just { isTapped = False }

                        else
                            Nothing
                    )
        , deck = deck
        , hand = Array.append gameArea.hand (freshDraw |> Array.fromList)
    }
