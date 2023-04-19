module Pages.Tank.Id_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Game exposing (TankId)
import Gen.Params.Tank.Id_ exposing (Params)
import Layout
import Page
import Request
import Shared
import View exposing (View)
import View.Common
import View.Tank


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req.params
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { tankId : TankId }


init : Params -> ( Model, Effect Msg )
init params =
    ( { tankId = String.toInt params.id |> Maybe.withDefault 0 }
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
view shared model =
    [ View.Common.storage
        { onClick = \id -> Shared.LoadFish model.tankId id |> ToShared
        }
        shared.game
    , Layout.textButton []
        { label = "Feed Fish"
        , onPress = Shared.FeedFish model.tankId |> ToShared |> Just
        }
    , View.Tank.toHtml
        { animationFrame = shared.animationFrame
        , storeFish = \id -> Shared.StoreFish model.tankId id |> ToShared
        , tankId = model.tankId
        }
        shared.game
    ]
        |> Layout.column [ Layout.gap 16, Layout.alignAtCenter ]
        |> Shared.view ToShared shared
