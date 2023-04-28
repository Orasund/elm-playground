module Pages.Home_ exposing (Model, Msg, page)

import Config
import Dict
import Effect exposing (Effect)
import Fish.Common
import Gen.Params.Home_ exposing (Params)
import Html
import Html.Attributes
import Layout
import Page
import Request
import Rule exposing (Pattern(..))
import Shared exposing (Msg(..), Tab(..))
import View exposing (View)
import View.Common


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init shared
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    ()


init : Shared.Model -> ( Model, Effect Msg )
init _ =
    ( ()
    , Effect.none
    )



-- UPDATE


type Msg
    = ToShared Shared.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToShared sharedMsg ->
            ( model, Effect.fromShared sharedMsg )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared () =
    [ View.Common.storage
        { onClick = \id -> Shared.SellFish id |> ToShared
        }
        shared.game
    , Layout.textButton []
        { label = "Sell all stored Fish"
        , onPress = SellAllFishInStorage |> ToShared |> Just
        }
    , View.Common.money shared.game.money
    , Layout.textButton []
        { label = "Buy Fish for " ++ String.fromInt Config.fishCost ++ " Money"
        , onPress = BuyFish |> ToShared |> Just
        }
    , Html.text "Discovered Breeds"
    , shared.game.breeds
        |> Dict.values
        |> List.map
            (\breed ->
                [ Fish.Common.new breed
                    |> View.Common.fishSprite []
                        { animationFrame = False }
                , Html.text breed.name
                ]
                    |> Layout.column []
            )
        |> Layout.row [ Layout.gap 8, Html.Attributes.style "width" "80px" ]
    ]
        |> Layout.column [ Layout.gap 16, Layout.alignAtCenter ]
        |> Shared.view ToShared shared
