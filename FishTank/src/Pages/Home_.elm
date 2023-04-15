module Pages.Home_ exposing (Model, Msg, page)

import Config
import Effect exposing (Effect)
import Gen.Params.Home_ exposing (Params)
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
    ]
        |> Layout.column [ Layout.gap 16, Layout.alignAtCenter ]
        |> Shared.view shared
