module FactoryCity.State.Playing exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Bag exposing (Bag)
import Element exposing (Element)
import FactoryCity.Data as Data
import FactoryCity.Data.Board as Board
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort, Item(..))
import FactoryCity.Data.Deck as Deck
import FactoryCity.Data.Game as Game exposing (EndCondition(..), Game)
import FactoryCity.View.Game as GameView
import FactoryCity.View.Header as HeaderView
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
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


type alias TransitionData =
    { game : Game
    , seed : Seed
    }


type alias Action =
    Action.Action Model Msg Never ()



----------------------
-- Init
----------------------


init : TransitionData -> ( Model, Cmd Msg )
init { game, seed } =
    ( ( { game = game
        , selected = Nothing
        , running = False
        , viewedCard = Nothing
        , initialSeed = seed
        , source = Wood
        , loopEvery = 10
        , stepCount = 10
        , shop =
            [ ( CellType.crate Scrap, 1000 )
            , ( CellType.crate Wood, 100 )
            , ( CellType.crate Stone, 100 )
            , ( CellType.crate Iron, 10 )
            ]
                |> List.map (Tuple.mapFirst CellType.containerSortToString)
                |> Bag.fromList
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
update msg (( { selected, stepCount, loopEvery, source, shop, money } as state, seed ) as model) =
    let
        defaultCase : Action
        defaultCase =
            Action.updating
                ( model, Cmd.none )
    in
    case msg of
        TimePassed ->
            Action.updating
                ( ( state
                        |> (\({ game } as x) ->
                                { state
                                    | stepCount = stepCount - 1
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
                                            |> Board.getInput
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
                                                                |> Deck.add (CellType.crate source)
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
                                                                |> Deck.add (CellType.crate source)
                                                    }
                                            }

                                else
                                    x
                           )
                        |> (\x -> { x | game = x.game |> Game.step })
                  , seed
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
            Action.updating
                ( ( case game.deck |> Deck.remove card of
                        Ok deck ->
                            { state
                                | money = money + price
                                , shop = shop |> Bag.insert 1 key
                                , game = { game | deck = deck }
                            }

                        Err () ->
                            state
                  , seed
                  )
                , Cmd.none
                )

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
            Action.updating
                ( ( if price <= money then
                        card
                            |> CellType.stringToContainerSort
                            |> Maybe.map buy
                            |> Maybe.withDefault state

                    else
                        state
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 (always TimePassed)



----------------------
-- View
----------------------


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper ( { stepCount, shop, money, game, selected, viewedCard, loopEvery }, _ ) =
    ( Nothing
    , [ HeaderView.view scale
            restartMsg
            stepCount
      , GameView.view
            money
            shop
            { scale = scale
            , selected = selected
            , sort = True
            , loopLength = loopEvery
            }
            { positionSelectedMsg = msgMapper << PositionSelected
            , selectedMsg = msgMapper << Selected
            , buyMsg = msgMapper << ClickedBuy
            , sellMsg = msgMapper << ClickedSell
            , changedLoopLengthMsg = msgMapper << ChangedLoopLength
            }
            game
      ]
    )
