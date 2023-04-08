module Main exposing (..)

import Browser
import Dict
import Fish exposing (FishId)
import Game exposing (Game)
import Layout
import Random exposing (Generator, Seed)
import Rule exposing (Pattern(..))
import Set
import Time
import View


type alias Random a =
    Generator a


type Tab
    = Tank Int


type alias Model =
    { seed : Seed
    , game : Game
    , tab : Tab
    , animationFrame : Bool
    }


type Msg
    = NextAnimationRequested
    | NextMovementRequested
    | MatingTriggered
    | FeedFish
    | StoreFish FishId
    | LoadFish FishId
    | SellFish FishId
    | SellAllFishInStorage
    | SwitchTab Tab


init : () -> ( Model, Cmd Msg )
init () =
    let
        r a =
            [ Horizontal, Vertical, TopDown, BottomUp ]
                |> List.map (Tuple.pair a)

        ( fish, seed ) =
            [ r True ++ r False
            , r True ++ r False
            ]
                |> List.foldl
                    (\rules ->
                        Random.andThen
                            (\l ->
                                Random.pair
                                    (Fish.generate rules)
                                    Game.randomLocation
                                    |> Random.map (\p -> p :: l)
                            )
                    )
                    (Random.constant [])
                |> (\gen -> Random.step gen (Random.initialSeed 42))

        tankId =
            0
    in
    ( { seed = seed
      , game =
            fish
                |> List.foldl (\( f, p ) -> Game.addFish p tankId f)
                    Game.new
      , tab = Tank tankId
      , animationFrame = False
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Fish Tank"
    , body =
        [ model.game.tanks
            |> Dict.keys
            |> List.map
                (\tankId ->
                    Layout.textButton []
                        { label = "Tank " ++ String.fromInt tankId
                        , onPress = Just (SwitchTab (Tank tankId))
                        }
                )
            |> Layout.row []
        , [ Layout.textButton []
                { label = "Feed Fish"
                , onPress = Just FeedFish
                }
          , Layout.textButton []
                { label = "Sell all stored Fish"
                , onPress = Just SellAllFishInStorage
                }
          ]
            |> Layout.row []
        , case model.tab of
            Tank tankId ->
                model.game
                    |> View.game
                        { animationFrame = model.animationFrame
                        , storeFish = StoreFish
                        , loadFish = LoadFish
                        , sellFish = SellFish
                        , tankId = tankId
                        }
        ]
    }


apply : Seed -> Random Model -> Model
apply s gen =
    Random.step gen s
        |> (\( m, seed ) -> { m | seed = seed })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextAnimationRequested ->
            ( { model | animationFrame = not model.animationFrame }
            , Cmd.none
            )

        FeedFish ->
            case model.tab of
                Tank tankId ->
                    ( { model
                        | game =
                            model.game
                                |> Game.feedTank tankId
                      }
                    , Cmd.none
                    )

        NextMovementRequested ->
            model.game.tanks
                |> Dict.toList
                |> List.concatMap
                    (\( tankId, dict ) ->
                        dict
                            |> Dict.values
                            |> List.concatMap Set.toList
                            |> List.map (Tuple.pair tankId)
                    )
                |> List.foldl
                    (\( tankId, fishId ) ->
                        Random.andThen (Game.act tankId fishId)
                    )
                    (Random.constant model.game)
                |> Random.map (\game -> { model | game = game })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))

        StoreFish fishId ->
            case model.tab of
                Tank tankId ->
                    ( { model | game = model.game |> Game.store tankId fishId }
                    , Cmd.none
                    )

        LoadFish fishId ->
            case model.tab of
                Tank tankId ->
                    model.game
                        |> Game.load tankId fishId
                        |> Random.map (\game -> { model | game = game })
                        |> apply model.seed
                        |> (\m -> ( m, Cmd.none ))

        SellFish fishId ->
            case model.tab of
                Tank tankId ->
                    ( { model
                        | game =
                            model.game
                                |> Game.sellFish tankId fishId
                      }
                    , Cmd.none
                    )

        SellAllFishInStorage ->
            case model.tab of
                Tank tankId ->
                    ( { model
                        | game =
                            model.game.storage
                                |> Set.foldl (Game.sellFish tankId)
                                    model.game
                      }
                    , Cmd.none
                    )

        SwitchTab tab ->
            ( { model | tab = tab }, Cmd.none )

        MatingTriggered ->
            model.game.tanks
                |> Dict.foldl
                    (\tankId dict rand ->
                        dict
                            |> Dict.values
                            |> List.concatMap Set.toList
                            |> List.foldl
                                (\fishId ->
                                    Random.andThen (Game.tryMating tankId fishId)
                                )
                                rand
                    )
                    (Random.constant model.game)
                |> Random.map (\game -> { model | game = game })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 (\_ -> NextAnimationRequested)
        , Time.every 1000 (\_ -> NextMovementRequested)
        , Time.every 1000 (\_ -> MatingTriggered)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
