module Pages.Home_ exposing (Model, Msg, page)

import Browser
import Dict
import Fish exposing (FishId)
import Game exposing (Game)
import Gen.Params.Home_ exposing (Params)
import Layout
import Page
import Random exposing (Generator, Seed)
import Request
import Rule exposing (Pattern(..))
import Set
import Shared
import Tank
import Time
import View exposing (View)
import View.Common as View


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


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


init : ( Model, Cmd Msg )
init =
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



-- UPDATE


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
                    model.game
                        |> Game.addFood tankId
                        |> Random.map (\game -> { model | game = game })
                        |> apply model.seed
                        |> (\m -> ( m, Cmd.none ))

        NextMovementRequested ->
            model.game.tanks
                |> Dict.toList
                |> List.concatMap
                    (\( tankId, tank ) ->
                        tank
                            |> Tank.fishIds
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
                    (\tankId tank rand ->
                        tank
                            |> Tank.fishIds
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 (\_ -> NextAnimationRequested)
        , Time.every 1000 (\_ -> NextMovementRequested)
        , Time.every 1000 (\_ -> MatingTriggered)
        ]



-- VIEW


view : Model -> View Msg
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
