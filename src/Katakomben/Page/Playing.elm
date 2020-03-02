module Katakomben.Page.Playing exposing (Model, Msg, TansitionData, init, subscriptions, update, view)

import Action
import Browser.Events as Events
import Element exposing (Element)
import Json.Decode as Decode
import Katakomben.Data.Game as Game exposing (Direction(..), Game, Msg(..))
import Katakomben.View.Game as Game
import Random exposing (Seed)


type alias Model =
    { seed : Seed
    , game : Game
    }


type Msg
    = Pressed (Maybe Direction)


type alias TansitionData =
    Seed


type alias Action =
    Action.Action Model Msg Never Never


init : TansitionData -> ( Model, Cmd Msg )
init seed =
    ( { seed = seed
      , game = Game.init Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> Action
update msg model =
    case msg of
        Pressed maybeDir ->
            case maybeDir of
                Just dir ->
                    let
                        ( game, seed ) =
                            Random.step (model.game |> Game.update (Chosen dir)) model.seed
                    in
                    Action.updating
                        ( { model
                            | game = game
                            , seed = seed
                          }
                        , Cmd.none
                        )

                Nothing ->
                    Action.updating ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Decode.field "key" Decode.string
        |> Decode.map
            (\string ->
                Pressed <|
                    case string of
                        "a" ->
                            Just Left

                        "A" ->
                            Just Left

                        "d" ->
                            Just Right

                        "D" ->
                            Just Right

                        "ArrowLeft" ->
                            Just Left

                        "ArrowRight" ->
                            Just Right

                        _ ->
                            Nothing
            )
        |> Events.onKeyDown


view : Model -> List (Element Msg)
view { game } =
    [ game
        |> Game.view
        |> Element.map (Just >> Pressed)
    ]
