module LittleWorldPuzzler.Data.Game exposing (EndCondition(..), Game, generator, json, step)

import Grid.Bordered as Grid
import Jsonstore exposing (Json)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board, columns, rows)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import Random exposing (Generator)
import Set exposing (Set)


type EndCondition
    = Lost
    | NewHighscore


type alias Game =
    { board : Board
    , deck : Deck
    , score : Int
    }


occuringTypes : Board -> Set String
occuringTypes board =
    board
        |> Board.values
        |> List.map CellType.toString
        |> Set.fromList


step : Set String -> Game -> ( Game, Set String )
step set ({ score } as game) =
    let
        board : Board
        board =
            game.board
                |> Grid.map (Automata.step (game.board |> Grid.toDict))
    in
    ( { game
        | board = board
        , score = score + 1
      }
    , set |> Set.union (occuringTypes board)
    )


generator : Generator Game
generator =
    Deck.generator
        |> Random.map
            (\deck ->
                { board =
                    Grid.empty { columns = columns, rows = rows }
                , deck = deck
                , score = 0
                }
            )



{------------------------
   Json
------------------------}


json : Json Game
json =
    Jsonstore.object (\board -> Game (board |> Board.fromList))
        |> Jsonstore.withList "board" Board.jsonTuple (.board >> Board.toList)
        |> Jsonstore.with "deck" Deck.json .deck
        |> Jsonstore.with "score" Jsonstore.int .score
        |> Jsonstore.toJson
