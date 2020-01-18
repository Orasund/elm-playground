module FactoryCity.Data.Game exposing (EndCondition(..), Game, generator, step)

import Dict exposing (Dict)
import FactoryCity.Automata as Automata exposing (ListRule)
import FactoryCity.Automata.Rule as Rule
import FactoryCity.Data.Board as Board exposing (Board, columns, rows)
import FactoryCity.Data.CellType as CellType exposing (CellType)
import FactoryCity.Data.Deck as Deck exposing (Deck, Selected(..))
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
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


occuringTypes : Board -> Set ( String, String )
occuringTypes board =
    board
        |> Board.values
        |> List.map CellType.toString
        |> Set.fromList


step : Set ( String, String ) -> Game -> ( Game, Set ( String, String ) )
step set ({ score } as game) =
    let
        boardStep : ListRule -> Dict Position CellType -> ( Board, Board ) -> ( Board, Board )
        boardStep listRule read ( b, remaining ) =
            remaining
                |> Grid.map (Automata.step (Automata.automata listRule) read)
                |> (\newRemaining ->
                        ( b |> Grid.union newRemaining
                        , remaining
                            |> Grid.filter
                                (\k v ->
                                    newRemaining
                                        |> Grid.toDict
                                        |> Dict.get k
                                        |> Maybe.map ((==) v)
                                        |> Maybe.withDefault False
                                )
                        )
                   )

        board : Board
        board =
            ( game.board, game.board )
                |> boardStep Rule.movables (game.board |> Grid.toDict)
                |> boardStep Rule.container (game.board |> Grid.toDict)
                |> boardStep Rule.burnable (game.board |> Grid.toDict)
                |> boardStep Rule.smeltable (game.board |> Grid.toDict)
                |> Tuple.first
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
