module FactoryCity.State.Playing exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Bag exposing (Bag)
import Element exposing (Element)
import FactoryCity.Data as Data
import FactoryCity.Data.Board as Board
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..), Item(..))
import FactoryCity.Data.Deck as Deck
import FactoryCity.Data.Game as Game exposing (EndCondition(..), Game)
import FactoryCity.Data.RemoteShop as RemoteShop
import FactoryCity.View.Game as GameView
import Grid.Bordered as Grid
import Grid.Direction as Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import Http exposing (Error(..))
import Process
import Random exposing (Seed)
import Set exposing (Set)
import Task
import Time
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type alias State =
    { game : Game
    , selected : Maybe ContainerSort
    , viewedCard : Maybe ContainerSort
    , initialSeed : Seed
    , running : Bool
    , source : Item
    , loopEvery : Int
    , stepCount : Int
    , shop : Bag String
    , money : Int
    , nextBugIn : Int
    }


type alias Model =
    ( State, Seed )


type Msg
    = Selected ContainerSort
    | PositionSelected Position
    | CardPlaced
    | CardSelected ContainerSort
    | TimePassed
    | ClickedBuy String
    | ClickedSell ContainerSort
    | ChangedLoopLength Int
    | ClickedCraft ContainerSort
    | GotShopResponse (Result Http.Error (Bag String))
    | Sync


type alias TransitionData =
    { shop : Bag String
    , seed : Seed
    , source : Item
    }


type alias Action =
    Action.Action Model Msg Never ()



----------------------
-- Init
----------------------


init : TransitionData -> ( Model, Cmd Msg )
init { shop, seed, source } =
    ( ( { game = Game.init source
        , selected = Nothing
        , running = False
        , viewedCard = Nothing
        , initialSeed = seed
        , source = source
        , loopEvery = 5
        , stepCount = 5
        , nextBugIn = Data.bugCycle
        , shop = shop
        , money = 0
        }
      , seed
      )
    , Cmd.none
    )



----------------------
-- Update
----------------------


play : Model -> Action
play ( { game } as state, seed ) =
    Action.updating
        ( ( { state
                | game = game
                , selected = Nothing
            }
          , seed
          )
        , Cmd.none
        )


playCard : ContainerSort -> Position -> Model -> Action
playCard containerSort position ( { game, initialSeed } as state, seed ) =
    play
        ( case game.deck |> Deck.remove containerSort of
            Ok deck ->
                case game.board |> Board.get position of
                    Just _ ->
                        state

                    Nothing ->
                        { state
                            | game =
                                { game
                                    | deck = deck
                                    , board =
                                        game.board
                                            |> Board.place position (containerSort |> CellType.fromCard)
                                }
                        }

            Err () ->
                state
        , seed
        )


removeCard : Position -> Model -> Action
removeCard position ( { game, initialSeed } as state, seed ) =
    let
        maybeCellType : Maybe CellType
        maybeCellType =
            game.board
                |> Board.get position
    in
    play
        ( { state
            | game =
                case maybeCellType of
                    Just cellType ->
                        { game
                            | deck =
                                game.deck
                                    |> Deck.add (cellType |> CellType.toCard)
                            , board = game.board |> Board.remove position
                        }

                    Nothing ->
                        game
          }
        , seed
        )


update : Msg -> Model -> Action
update msg (( { selected, stepCount, loopEvery, source, nextBugIn, shop, money } as state, seed ) as model) =
    let
        defaultCase : Action
        defaultCase =
            Action.updating
                ( model, Cmd.none )
    in
    case msg of
        TimePassed ->
            Action.updating
                ( state
                    |> (\({ game } as x) ->
                            { state
                                | stepCount = stepCount - 1
                                , nextBugIn = nextBugIn - 1
                                , game =
                                    { game
                                        | deck =
                                            game.board
                                                |> Board.getOutput
                                                |> List.foldl Deck.add game.deck
                                        , board = game.board |> Board.unload
                                    }
                            }
                       )
                    |> (\({ game } as x) ->
                            if x.stepCount <= 0 then
                                case
                                    game.board
                                        |> Board.getInput source
                                        |> List.foldl
                                            (\cell ->
                                                Result.andThen
                                                    (Deck.remove cell)
                                            )
                                            (Ok game.deck)
                                of
                                    Ok deck ->
                                        { x
                                            | stepCount = loopEvery
                                            , game =
                                                { game
                                                    | deck =
                                                        deck
                                                    , board =
                                                        game.board
                                                            |> Board.refill
                                                }
                                        }

                                    Err () ->
                                        { x
                                            | stepCount = loopEvery
                                            , game =
                                                { game
                                                    | deck =
                                                        game.deck
                                                }
                                        }

                            else
                                x
                       )
                    |> (\({ game } as x) ->
                            { x
                                | game =
                                    { game
                                        | board =
                                            game.board
                                                |> Grid.map
                                                    (\pos ->
                                                        Maybe.map
                                                            (\cell ->
                                                                if
                                                                    ([ Up, Left, Right, Down ]
                                                                        |> List.filterMap
                                                                            (\dir ->
                                                                                case
                                                                                    game.board
                                                                                        |> Grid.get
                                                                                            (pos
                                                                                                |> Position.move 1
                                                                                                    (case dir of
                                                                                                        Right ->
                                                                                                            Up

                                                                                                        Up ->
                                                                                                            Right

                                                                                                        Down ->
                                                                                                            Left

                                                                                                        Left ->
                                                                                                            Down
                                                                                                    )
                                                                                            )
                                                                                of
                                                                                    Ok (Just { sort }) ->
                                                                                        case sort of
                                                                                            Movable _ { from } ->
                                                                                                if from == dir then
                                                                                                    Just ()

                                                                                                else
                                                                                                    Nothing

                                                                                            _ ->
                                                                                                Nothing

                                                                                    _ ->
                                                                                        Nothing
                                                                            )
                                                                        |> List.length
                                                                    )
                                                                        > 1
                                                                then
                                                                    CellType.fromCard <| CellType.crate Scrap

                                                                else
                                                                    cell
                                                            )
                                                    )
                                    }
                            }
                       )
                    |> (\x -> { x | game = x.game |> Game.step })
                    |> (\({ game } as s) ->
                            if s.nextBugIn <= 0 then
                                seed
                                    |> Random.step
                                        (Random.map2
                                            (\x y ->
                                                { s
                                                    | nextBugIn = Data.bugCycle
                                                    , game =
                                                        { game
                                                            | board =
                                                                game.board
                                                                    |> Grid.ignoringErrors
                                                                        (Grid.update ( x, y ) (always <| Ok <| Just <| { item = Nothing, sort = Bug }))
                                                        }
                                                }
                                            )
                                            (Random.int 0 3)
                                            (Random.int 0 3)
                                        )

                            else
                                ( s, seed )
                       )
                , Cmd.none
                )

        Selected select ->
            Action.updating
                ( ( { state
                        | selected =
                            if selected == Just select then
                                Nothing

                            else
                                Just select
                    }
                  , seed
                  )
                , Cmd.none
                )

        PositionSelected position ->
            case selected of
                Just cellType ->
                    playCard cellType position model

                Nothing ->
                    removeCard position model

        CardPlaced ->
            Action.updating
                ( ( { state
                        | game = state.game |> Game.step
                    }
                  , seed
                  )
                , Cmd.none
                )

        CardSelected cellType ->
            Action.updating
                ( ( { state
                        | viewedCard = Just cellType
                    }
                  , seed
                  )
                , Cmd.none
                )

        ClickedSell card ->
            let
                key : String
                key =
                    card |> CellType.containerSortToString

                price : Int
                price =
                    Data.maxPrice // (shop |> Bag.count key |> (+) 1)

                game : Game
                game =
                    state.game
            in
            case game.deck |> Deck.remove card of
                Ok deck ->
                    case card of
                        Crate i ->
                            Action.updating
                                ( ( { state
                                        | money = money + price
                                        , shop = shop |> Bag.insert 1 key
                                        , game = { game | deck = deck }
                                    }
                                  , seed
                                  )
                                , Task.attempt GotShopResponse
                                    (RemoteShop.insert i
                                        |> Task.andThen
                                            (\() ->
                                                RemoteShop.sync
                                            )
                                    )
                                )

                        _ ->
                            Action.updating
                                ( ( { state
                                        | money = money + price
                                        , shop =
                                            shop
                                                |> Bag.insert 1
                                                    (Crate Scrap |> CellType.containerSortToString)
                                        , game = { game | deck = deck }
                                    }
                                  , seed
                                  )
                                , Task.attempt GotShopResponse
                                    (RemoteShop.insert Scrap
                                        |> Task.andThen
                                            (\() ->
                                                RemoteShop.sync
                                            )
                                    )
                                )

                Err () ->
                    Action.updating ( ( state, seed ), Cmd.none )

        ClickedBuy card ->
            let
                price : Int
                price =
                    Data.maxPrice // (shop |> Bag.count card)

                game : Game
                game =
                    state.game

                buy : ContainerSort -> State
                buy containerSort =
                    { state
                        | money = money - price
                        , shop = shop |> Bag.remove 1 card
                        , game = { game | deck = game.deck |> Deck.add containerSort }
                    }
            in
            if price <= money then
                card
                    |> CellType.stringToContainerSort
                    |> Maybe.andThen
                        (\c ->
                            case c of
                                Crate i ->
                                    Just <|
                                        Action.updating
                                            ( ( c |> buy
                                              , seed
                                              )
                                            , Task.attempt GotShopResponse
                                                (RemoteShop.remove i
                                                    |> Task.andThen
                                                        (\() ->
                                                            RemoteShop.sync
                                                        )
                                                )
                                            )

                                _ ->
                                    Nothing
                        )
                    |> Maybe.withDefault
                        (Action.updating
                            ( ( state
                              , seed
                              )
                            , Cmd.none
                            )
                        )

            else
                Action.updating
                    ( ( state
                      , seed
                      )
                    , Cmd.none
                    )

        ChangedLoopLength int ->
            Action.updating
                ( ( { state | loopEvery = int }
                  , seed
                  )
                , Cmd.none
                )

        ClickedCraft card ->
            let
                cost : Bag String
                cost =
                    card |> CellType.craftingCost

                game =
                    state.game
            in
            Action.updating
                ( ( case
                        cost
                            |> Bag.toList
                            |> List.foldl
                                (\( cell, n ) ->
                                    Result.andThen
                                        (\d ->
                                            if n <= (d |> Bag.count cell) then
                                                d
                                                    |> Bag.remove n cell
                                                    |> Ok

                                            else
                                                Err ()
                                        )
                                )
                                (Ok game.deck)
                    of
                        Ok deck ->
                            { state
                                | game =
                                    { game
                                        | deck =
                                            deck
                                                |> Deck.add card
                                    }
                                , selected = Just card
                            }

                        Err () ->
                            { state | selected = Just card }
                  , seed
                  )
                , Cmd.none
                )

        GotShopResponse result ->
            case result of
                Ok s ->
                    Action.updating
                        ( ( { state | shop = s }
                          , seed
                          )
                        , Cmd.none
                        )

                Err _ ->
                    Action.updating
                        ( ( state
                          , seed
                          )
                        , Cmd.none
                        )

        Sync ->
            Action.updating
                ( ( state
                  , seed
                  )
                , RemoteShop.sync
                    |> Task.attempt GotShopResponse
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 (always TimePassed)
        , Time.every 60000 (always Sync)
        ]



----------------------
-- View
----------------------


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper ( { stepCount, nextBugIn, shop, money, game, selected, viewedCard, loopEvery }, _ ) =
    ( Nothing
    , [ GameView.view
            { counter = stepCount
            , shop = shop
            , money = money
            , scale = scale
            , selected = selected
            , sort = True
            , loopLength = loopEvery
            , positionSelectedMsg = msgMapper << PositionSelected
            , selectedMsg = msgMapper << Selected
            , buyMsg = msgMapper << ClickedBuy
            , sellMsg = msgMapper << ClickedSell
            , changedLoopLengthMsg = msgMapper << ChangedLoopLength
            , craftMsg = msgMapper << ClickedCraft
            , nextBugIn = nextBugIn
            }
            game
      ]
    )
