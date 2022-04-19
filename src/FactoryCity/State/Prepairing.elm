module FactoryCity.State.Prepairing exposing (Model, Msg(..), init, update, view)

import Action
import Bag exposing (Bag)
import Browser.Dom as Dom exposing (Viewport)
import Element exposing (Element)
import Element.Font as Font
import FactoryCity.Data.RemoteShop as RemoteShop
import FactoryCity.View.Text as Text
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading
import Http exposing (Error(..))
import Random exposing (Seed)
import Task



----------------------
-- Model
----------------------


type alias Model =
    { scale : Maybe Float
    , seed : Maybe Seed
    , shop : Maybe (Bag String)
    , error : Maybe Http.Error
    }


type Msg
    = GotSeed Seed
    | GotShopResponse (Result Http.Error (Bag String))
    | GotViewport Viewport


type alias Action =
    Action.Action
        Model
        Never
        { scale : Float
        , seed : Seed
        , shop : Bag String
        }
        Never


init : ( Model, Cmd Msg )
init =
    ( { scale = Nothing
      , seed = Nothing
      , shop = Nothing
      , error = Nothing
      }
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed

        {--, Task.attempt GotShopResponse RemoteShop.sync --}
        , Task.perform GotViewport Dom.getViewport
        ]
    )



----------------------
-- Update
----------------------


validate : Model -> Action
validate model =
    Maybe.map3
        (\scale seed shop ->
            Action.transitioning
                { scale = scale
                , seed = seed
                , shop = shop
                }
        )
        model.scale
        model.seed
        model.shop
        |> Maybe.withDefault (Action.updating ( model, Cmd.none ))


update : ({ height : Float, width : Float } -> Float) -> Msg -> Model -> Action
update calcScale msg model =
    case msg of
        GotSeed seed ->
            { model | seed = Just seed }
                |> validate

        GotShopResponse result ->
            case result of
                Ok shop ->
                    { model | shop = Just shop }
                        |> validate

                Err error ->
                    Action.updating ( { model | error = Just error }, Cmd.none )

        GotViewport v ->
            { model
                | scale =
                    { width = v.viewport.width, height = v.viewport.height }
                        |> calcScale
                        |> Just
            }
                |> validate


view :
    Model
    -> ( Maybe ( Element msg, Element msg ), List (Element msg) )
view model =
    ( Nothing
    , List.singleton <|
        Element.column
            (Grid.simple
                ++ [ Element.width <| Element.shrink
                   , Element.centerY
                   ]
            )
            [ Text.colored (round <| 150) "🏭"
            , Element.column
                (Heading.h1
                    ++ [ Element.centerX
                       , Font.center
                       ]
                )
              <|
                [ Element.text "Factory"
                , Element.text "City"
                ]
            , model.error
                |> Maybe.map
                    (\error ->
                        Element.el (Card.large ++ Color.warning) <|
                            Element.text <|
                                "Error:"
                                    ++ (case error of
                                            BadUrl string ->
                                                "bad url: " ++ string

                                            Timeout ->
                                                "timeout"

                                            NetworkError ->
                                                "network error"

                                            BadStatus int ->
                                                "bad status: " ++ String.fromInt int

                                            BadBody string ->
                                                "bad body: " ++ string
                                       )
                    )
                |> Maybe.withDefault
                    (Element.el Card.large <|
                        Element.text "Loading..."
                    )
            ]
    )
