module FactoryCity.Data.Game exposing (Game, Msg(..), Tab(..), craft, init, playCard, removeCard, step, tabToLabel, tick, update)

import Bag exposing (Bag)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import FactoryCity.Automata as Automata exposing (ListRule)
import FactoryCity.Automata.Rule as Rule
import FactoryCity.Data as Data
import FactoryCity.Data.Board as Board exposing (Board, columns, rows)
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..), RemovableSort(..))
import FactoryCity.Data.Deck as Deck exposing (Deck)
import FactoryCity.Data.Item as Item exposing (Item(..))
import Grid.Bordered as Grid
import Http
import Position
import Random exposing (Generator)
import Set exposing (Set)
import Task


type Tab
    = ShopTab
    | CraftingTab
    | GameTab
    | MachineTab
    | DetailsTab


tabToLabel : Tab -> String
tabToLabel tab =
    case tab of
        ShopTab ->
            "Shop"

        CraftingTab ->
            "Craft"

        GameTab ->
            "Game"

        MachineTab ->
            "Machine"

        DetailsTab ->
            "Details"


type Msg
    = ClickedSell ContainerSort
    | ClickedBuy Item Int
    | GotShopResponse (Result Http.Error (Bag String))
    | ChangedLoopLength Int
    | Sync
    | ToggledBuyRegularly Item
    | ToggledSellRegularly Item


type alias Game =
    { board : Board
    , deck : Deck
    , score : Int
    , source : Item
    , money : Int
    , loopEvery : Int
    , stepCount : Int
    , shop : Bag String
    , nextBugIn : Int
    , bugCycle : Int
    , shoppingList : Set String
    , sellingList : Set String
    }


playCard : ContainerSort -> ( Int, Int ) -> Game -> Game
playCard containerSort position game =
    case game.deck |> Deck.remove containerSort 1 of
        Ok deck ->
            case game.board |> Board.get position of
                Just _ ->
                    game

                Nothing ->
                    { game
                        | deck = deck
                        , board =
                            game.board
                                |> Board.place position (containerSort |> CellType.fromCard)
                    }

        Err () ->
            game


removeCard : ( Int, Int ) -> Game -> Game
removeCard position game =
    let
        maybeCellType : Maybe CellType
        maybeCellType =
            game.board
                |> Board.get position
    in
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


tick : Game -> Generator ( Game, Cmd Msg )
tick =
    (\game ->
        { game
            | stepCount = game.stepCount - 1
            , nextBugIn = game.nextBugIn - 1
            , deck =
                game.board
                    |> Board.getOutput
                    |> List.foldl Deck.add game.deck
            , board = game.board |> Board.unload
        }
    )
        >> (\game ->
                if game.stepCount <= 0 then
                    case
                        game.board
                            |> Board.getInput game.source
                            |> List.foldl
                                (\cell ->
                                    Result.andThen
                                        (Deck.remove cell 1)
                                )
                                (Ok game.deck)
                    of
                        Ok deck ->
                            { game
                                | stepCount = game.loopEvery
                                , deck =
                                    deck
                                , board =
                                    game.board
                                        |> Board.refill
                            }

                        Err () ->
                            { game
                                | deck =
                                    game.deck
                                , stepCount = game.loopEvery
                            }

                else
                    game
           )
        >> (\game ->
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
                                                                            |> Position.add
                                                                                ((case dir of
                                                                                    Right ->
                                                                                        Up

                                                                                    Up ->
                                                                                        Right

                                                                                    Down ->
                                                                                        Left

                                                                                    Left ->
                                                                                        Down
                                                                                 )
                                                                                    |> Direction.toCoord
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
                                                CellType.fromCard <| Removable Trash

                                            else
                                                cell
                                        )
                                )
                }
           )
        >> step
        >> (\game ->
                if game.nextBugIn <= 0 then
                    Random.map2
                        (\x y ->
                            { game
                                | board =
                                    game.board
                                        |> Grid.ignoringErrors
                                            (Grid.update ( x, y )
                                                (always <| Ok <| Just <| { item = Nothing, sort = Removable Bug })
                                            )
                                , nextBugIn = game.bugCycle - 1
                                , bugCycle = game.bugCycle - 1
                            }
                        )
                        (Random.int 0 3)
                        (Random.int 0 3)

                else
                    Random.constant game
           )
        >> Random.map
            (\game ->
                ( game
                , if game.stepCount <= 1 then
                    game.shoppingList
                        |> Set.toList
                        |> List.filterMap
                            (\string ->
                                string
                                    |> Item.stringToItem
                                    |> Maybe.map
                                        (\a ->
                                            Task.perform
                                                (\() -> ClickedBuy a 1)
                                                (Task.succeed ())
                                        )
                            )
                        |> Cmd.batch

                  else
                    game.sellingList
                        |> Set.toList
                        |> List.filterMap
                            (\string ->
                                string
                                    |> Item.stringToItem
                                    |> Maybe.map
                                        (\a ->
                                            Task.perform
                                                (\() ->
                                                    ClickedSell (Crate a)
                                                )
                                                (Task.succeed ())
                                        )
                            )
                        |> Cmd.batch
                )
            )


craft : ContainerSort -> Game -> Game
craft card game =
    let
        cost : Bag String
        cost =
            card |> CellType.craftingCost
    in
    case
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
            { game
                | deck =
                    deck
                        |> Deck.add card
            }

        Err () ->
            game


step : Game -> Game
step ({ score } as game) =
    let
        boardStep : ListRule -> Dict ( Int, Int ) CellType -> ( Board, Board ) -> ( Board, Board )
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


init : Item -> Bag String -> Game
init item shop =
    { board =
        [ ( ( 1, 1 ), { item = Nothing, sort = CellType.crate item } )
        , ( ( 2, 1 ), { item = Nothing, sort = CellType.belt { from = Up, to = Down } } )
        , ( ( 3, 1 ), { item = Nothing, sort = CellType.output } )
        ]
            |> Grid.fromList { columns = columns, rows = rows }
    , deck = Deck.init
    , score = 0
    , source = item
    , money = 0
    , loopEvery = 5
    , stepCount = 5
    , nextBugIn = Data.maxBugCycle
    , shop = shop
    , bugCycle = Data.maxBugCycle
    , shoppingList = Set.empty
    , sellingList = Set.empty
    }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    let
        defaultCase : ( Game, Cmd Msg )
        defaultCase =
            ( game, Cmd.none )
    in
    case msg of
        ClickedSell card ->
            case game.deck |> Deck.remove card 1 of
                Ok deck ->
                    case card of
                        Crate i ->
                            let
                                key : String
                                key =
                                    i |> Item.itemToString

                                price : Int
                                price =
                                    Data.maxPrice // (game.shop |> Bag.count key |> (+) 1)
                            in
                            ( { game
                                | shop = game.shop |> Bag.insert 1 key
                                , money = game.money + price
                                , deck = deck
                              }
                            , Cmd.none
                              {--Task.attempt GotShopResponse
                                (RemoteShop.insert i 1)--}
                            )

                        _ ->
                            let
                                key : String
                                key =
                                    Scrap |> Item.itemToString

                                price : Int
                                price =
                                    Data.maxPrice // (game.shop |> Bag.count key |> (+) 1)
                            in
                            ( { game
                                | shop = game.shop |> Bag.insert 1 key
                                , deck = deck
                                , money = game.money + price
                              }
                            , Cmd.none
                              {--Task.attempt GotShopResponse
                                (RemoteShop.insert Scrap 1)--}
                            )

                Err () ->
                    defaultCase

        ClickedBuy item _ ->
            let
                key : String
                key =
                    item |> Item.itemToString

                price : Int
                price =
                    Data.maxPrice // (game.shop |> Bag.count key)
            in
            if price <= game.money then
                ( { game
                    | shop = game.shop |> Bag.remove 1 key
                    , money = game.money - price
                    , deck = game.deck |> Deck.add (Crate item)
                  }
                , Cmd.none
                  {--Task.attempt GotShopResponse
                    (RemoteShop.remove item n)--}
                )

            else
                defaultCase

        GotShopResponse result ->
            ( case result of
                Ok s ->
                    { game | shop = s }

                Err _ ->
                    game
            , Cmd.none
            )

        ChangedLoopLength int ->
            ( { game | loopEvery = int }
            , Cmd.none
            )

        Sync ->
            ( game
            , Cmd.none
              {--RemoteShop.sync
                |> Task.attempt GotShopResponse--}
            )

        ToggledBuyRegularly item ->
            if game.shoppingList |> Set.member (item |> Item.itemToString) then
                ( { game
                    | shoppingList =
                        game.shoppingList
                            |> Set.remove (item |> Item.itemToString)
                  }
                , Cmd.none
                )

            else
                ( { game
                    | shoppingList =
                        game.shoppingList
                            |> Set.insert (item |> Item.itemToString)
                  }
                , Cmd.none
                )

        ToggledSellRegularly item ->
            if game.sellingList |> Set.member (item |> Item.itemToString) then
                ( { game
                    | sellingList =
                        game.sellingList
                            |> Set.remove (item |> Item.itemToString)
                  }
                , Cmd.none
                )

            else
                ( { game
                    | sellingList =
                        game.sellingList
                            |> Set.insert (item |> Item.itemToString)
                  }
                , Cmd.none
                )
