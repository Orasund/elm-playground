module LittleWorldPuzzler.Data.Game exposing (EndCondition(..), Game, decoder, encode, generator, step)

import Framework.Modifier exposing (Modifier(..))
import Grid.Bordered as Grid
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board, columns, rows)
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import Random exposing (Generator)


type EndCondition
    = Lost
    | NewHighscore


type alias Game =
    { board : Board
    , deck : Deck
    , score : Int
    }


step : Game -> Game
step ({ board, score } as game) =
    { game
        | board =
            board
                |> Grid.map (Automata.step (board |> Grid.toDict))
        , score = score + 1
    }


generator : Generator Game
generator =
    [ Wood
    , Wood
    , Wood
    , Wood
    , Water
    , Water
    , Water
    , Water
    , Stone
    , Fire
    ]
        |> Deck.fromList
        |> Deck.shuffle
        |> Random.map
            (\deck ->
                { board =
                    Grid.empty { columns = columns, rows = rows }
                , deck = deck
                , score = 0
                }
            )



{------------------------
   Decoder
------------------------}


decoder : Decoder Game
decoder =
    D.map3
        (\board deck score ->
            { board = board
            , deck = deck
            , score = score
            }
        )
        ((D.map
            (Maybe.withDefault <|
                Grid.empty { columns = columns, rows = rows }
            )
            << D.maybe
            << D.field "board"
         )
         <|
            Board.decoder
        )
        (D.field "deck" Deck.decoder)
        (D.field "score" D.int)



{------------------------
   Encoder
------------------------}


encode : Game -> Value
encode { board, deck, score } =
    E.object
        [ ( "board", Board.encode board )
        , ( "deck", Deck.encode deck )
        , ( "score", E.int score )
        ]
