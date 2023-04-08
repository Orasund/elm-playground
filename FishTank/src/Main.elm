module Main exposing (..)

import Array
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


type alias Model =
    { seed : Seed
    , game : Game
    , animationFrame : Bool
    }


type Msg
    = NextAnimationRequested
    | NextMovementRequested
    | AddFish
    | StoreFish FishId
    | LoadFish FishId
    | SellFish FishId
    | SellAllFishInStorage


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
    in
    ( { seed = seed
      , game =
            fish
                |> List.foldl (\( f, p ) -> Game.addFish p f)
                    Game.new
      , animationFrame = False
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Fish Tank"
    , body =
        [ [ Layout.textButton []
                { label = "Add Fish"
                , onPress = Just AddFish
                }
          , Layout.textButton []
                { label = "Sell all Fish"
                , onPress = Just SellAllFishInStorage
                }
          ]
            |> Layout.row []
        , model.game
            |> View.game
                { animationFrame = model.animationFrame
                , storeFish = StoreFish
                , loadFish = LoadFish
                , sellFish = SellFish
                }
        ]
    }


pickTwo : List a -> Random (Maybe ( a, a ))
pickTwo list =
    let
        arr =
            Array.fromList list
    in
    Random.map2
        (\i1 i2 ->
            Maybe.map2 Tuple.pair
                (Array.get
                    (if i1 > i2 then
                        i2

                     else
                        i2 + 1
                    )
                    arr
                )
                (Array.get i1 arr)
        )
        (Random.int 0 (List.length list - 1))
        (Random.int 0 (List.length list - 2))


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

        AddFish ->
            model.game.tank
                |> Dict.values
                |> List.concatMap Set.toList
                |> pickTwo
                |> Random.andThen
                    (\maybe ->
                        maybe
                            |> Maybe.map
                                (\( fish1, fish2 ) ->
                                    Game.randomLocation
                                        |> Random.andThen
                                            (\location ->
                                                model.game
                                                    |> Game.breed location ( fish1, fish2 )
                                            )
                                )
                            |> Maybe.withDefault (Random.constant model.game)
                    )
                |> Random.map (\game -> { model | game = game })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))

        NextMovementRequested ->
            model.game.tank
                |> Dict.values
                |> List.concatMap Set.toList
                |> List.foldl
                    (\fishId ->
                        Random.andThen (Game.act fishId)
                    )
                    (Random.constant model.game)
                |> Random.map (\game -> { model | game = game })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))

        StoreFish fishId ->
            ( { model | game = model.game |> Game.store fishId }, Cmd.none )

        LoadFish fishId ->
            model.game
                |> Game.load fishId
                |> Random.map (\game -> { model | game = game })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))

        SellFish fishId ->
            ( { model
                | game =
                    model.game
                        |> Game.sellFish fishId
              }
            , Cmd.none
            )

        SellAllFishInStorage ->
            ( { model
                | game =
                    model.game.storage
                        |> Set.foldl Game.sellFish model.game
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 (\_ -> NextAnimationRequested)
        , Time.every 1000 (\_ -> NextMovementRequested)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
