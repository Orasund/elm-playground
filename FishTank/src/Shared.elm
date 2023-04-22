module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , Tab(..)
    , init
    , subscriptions
    , update
    , view
    )

import Action exposing (Action(..))
import Config
import Dict
import Fish.Common exposing (FishId)
import Game exposing (Game, TankId)
import Gen.Route
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Json
import Layout
import Random exposing (Generator, Seed)
import Request exposing (Request)
import Rule exposing (Pattern(..))
import Set
import Tank
import Time
import View exposing (View)
import View.Overlay


type alias Flags =
    Json.Value


type alias Random a =
    Generator a


type Tab
    = TankTab Int


type alias Model =
    { seed : Seed
    , game : Game
    , animationFrame : Bool
    , actions : List Action
    }


type Msg
    = NextAnimationRequested
    | NextMovementRequested
    | FeedFish TankId
    | StoreFish TankId FishId
    | LoadFish TankId FishId
    | SellFish FishId
    | BuyFish
    | SellAllFishInStorage
    | NextActionRequested
    | GotSeed Seed


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { seed = Random.initialSeed 42
      , game = Game.new
      , animationFrame = False
      , actions = []
      }
    , Random.generate GotSeed Random.independentSeed
    )


apply : Seed -> Random Model -> Model
apply s gen =
    Random.step gen s
        |> (\( m, seed ) -> { m | seed = seed })


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        NextAnimationRequested ->
            ( { model | animationFrame = not model.animationFrame }
            , Cmd.none
            )

        FeedFish tankId ->
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
                    (\( tankId, fishId ) rand ->
                        rand
                            |> Action.randAndThen (Game.tryMating tankId fishId)
                            |> Action.randMap (Game.act tankId fishId)
                    )
                    (Random.constant ( model.game, [] ))
                |> Random.map (\( game, actions ) -> { model | game = game, actions = actions ++ model.actions })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))

        StoreFish tankId fishId ->
            if (model.game.storage |> Set.size) < 5 then
                ( { model | game = model.game |> Game.store tankId fishId }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        LoadFish tankId fishId ->
            model.game
                |> Game.load tankId fishId
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
                        |> Set.foldl Game.sellFish
                            model.game
              }
            , Cmd.none
            )

        BuyFish ->
            model.game
                |> Game.buyFish
                |> Random.map (\game -> { model | game = game })
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))

        NextActionRequested ->
            case model.actions of
                (NewBreed _) :: tail ->
                    model.game
                        |> (\game ->
                                ( { model
                                    | actions = tail
                                    , game = game
                                  }
                                , Cmd.none
                                )
                           )

                [] ->
                    ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ model =
    if model.actions == [] then
        Sub.batch
            [ Time.every 500 (\_ -> NextAnimationRequested)
            , Time.every 1000 (\_ -> NextMovementRequested)
            ]

    else
        Sub.none


view : (Msg -> msg) -> Model -> Html msg -> View msg
view fun model content =
    { title = "Fish Tank"
    , body =
        [ [ [ model.game.tanks
                |> Dict.toList
                |> List.map
                    (\( tankId, tank ) ->
                        Html.text
                            ("Tank "
                                ++ String.fromInt tankId
                                ++ " ("
                                ++ (tank |> Tank.getFish |> Dict.size |> String.fromInt)
                                ++ "/"
                                ++ String.fromInt Config.maxFishPerTank
                                ++ ")"
                            )
                            |> Layout.linkTo []
                                (Gen.Route.Tank__Id_ { id = String.fromInt tankId }
                                    |> Gen.Route.toHref
                                )
                    )
                |> (++)
                    [ Html.text "Market"
                        |> Layout.linkTo []
                            (Gen.Route.Home_ |> Gen.Route.toHref)
                    ]
                |> Layout.row [ Layout.gap 8 ]
            , content
            ]
                |> Layout.column
                    [ Layout.alignAtCenter
                    , Html.Attributes.style "height" "620px"
                    ]
                |> Layout.el
                    ([ Html.Attributes.style "height" "100%"
                     , Html.Attributes.style "width" "100%"
                     ]
                        ++ Layout.centered
                    )
          , model.actions
                |> List.head
                |> View.Overlay.toHtml { onSubmit = fun NextActionRequested }
                    model.game
          ]
            |> Html.div
                [ Html.Attributes.style "position" "relative"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ]
        , """:root,body {
            height:100%;
            margin:0px;
            }
            
            body {
                background-color:#83bb7e;
            }

            .feedButton {
                aspect-ratio: 1;
                width: 50px;
                background-color: yellow;
                border: 1px solid black;
                position:relative;
                top: 0px;
                transition: top 0.1s;
            }

            button:hover {
                filter: brightness(0.9);
                top: -8px;
            }

            button:active {
                filter: brightness(0.75);
                top: 16px;
            }
            """
            |> Html.text
            |> List.singleton
            |> Html.node "style" []
        ]
    }
