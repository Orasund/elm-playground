module FactoryCity.Data.Game exposing (EndCondition(..), Game, init, step)

import Dict exposing (Dict)
import FactoryCity.Automata as Automata exposing (ListRule)
import FactoryCity.Automata.Rule as Rule
import FactoryCity.Data.Board exposing (Board, columns, rows)
import FactoryCity.Data.CellType as CellType exposing (CellType)
import FactoryCity.Data.Deck as Deck exposing (Deck)
import FactoryCity.Data.Item exposing (Item)
import Grid.Bordered as Grid
import Grid.Direction exposing (Direction(..))
import Grid.Position exposing (Position)



--import Set exposing (Set)


type EndCondition
    = Lost
    | NewHighscore


type alias Game =
    { board : Board
    , deck : Deck
    , score : Int
    }



{- occuringTypes : Board -> Set ( String, String )
   occuringTypes board =
       board
           |> Board.values
           |> List.map CellType.toString
           |> Set.fromList
-}


step : Game -> Game
step ({ score } as game) =
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
                |> boardStep Rule.shreddable (game.board |> Grid.toDict)
                |> boardStep Rule.pressable (game.board |> Grid.toDict)
                |> boardStep Rule.merger (game.board |> Grid.toDict)
                |> boardStep Rule.output (game.board |> Grid.toDict)
                |> Tuple.first
    in
    { game
        | board = board
        , score = score + 1
    }


init : Item -> Game
init item =
    { board =
        [ ( ( 1, 1 ), { item = Nothing, sort = CellType.crate item } )
        , ( ( 2, 1 ), { item = Nothing, sort = CellType.belt { from = Up, to = Down } } )
        , ( ( 3, 1 ), { item = Nothing, sort = CellType.output } )
        ]
            |> Grid.fromList { columns = columns, rows = rows }
    , deck = Deck.init
    , score = 0
    }
