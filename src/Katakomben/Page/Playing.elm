module Katakomben.Page.Playing exposing (Model, Msg, TansitionData, init, subscriptions, update, view)

import Action
import Browser.Events as Events
import Element exposing (Element)
import Json.Decode as Decode
import Katakomben.Data.Game as Game exposing (Direction(..), Game, Msg(..))
import Katakomben.View.Card as Card
import Katakomben.View.Game as Game
import Process
import Random exposing (Seed)
import Task


type alias Model =
    { seed : Seed
    , game : Game
    , selected : Maybe Direction
    , showAnimation : Bool
    }


type Msg
    = Pressed (Maybe Direction)
    | Selected Direction
    | MouseOver (Maybe Direction)
    | ActivateAnimation


type alias TansitionData =
    Seed


type alias Action =
    Action.Action Model Msg Never Never


init : TansitionData -> ( Model, Cmd Msg )
init seed =
    ( { seed = seed
      , game = Game.init Nothing
      , selected = Nothing
      , showAnimation = True
      }
    , Cmd.none
    )


update : Msg -> Model -> Action
update msg model =
    let
        select : Direction -> Model
        select dir =
            let
                ( game, seed ) =
                    Random.step (model.game |> Game.update (Chosen dir)) model.seed
            in
            { model
                | game = game
                , seed = seed
                , selected = Nothing
                , showAnimation = False
            }
    in
    case msg of
        Pressed maybeDir ->
            case maybeDir of
                Just dir ->
                    Action.updating
                        ( select dir
                        , Process.sleep 10
                            |> Task.perform (always ActivateAnimation)
                        )

                Nothing ->
                    Action.updating
                        ( model
                        , Cmd.none
                        )

        Selected dir ->
            Action.updating
                ( select dir
                , Cmd.batch
                    [ Process.sleep 10
                        |> Task.perform (always ActivateAnimation)
                    , Process.sleep 200
                        |> Task.perform (always (MouseOver (Just dir)))
                    ]
                )

        MouseOver maybeDir ->
            Action.updating
                ( { model | selected = maybeDir }
                , Cmd.none
                )

        ActivateAnimation ->
            Action.updating
                ( { model | showAnimation = True }
                , Cmd.none
                )


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
view { game, selected, showAnimation } =
    [ game
        |> Game.view
            { selected = selected
            , showAnimation = showAnimation
            }
        |> Element.map
            (\msg ->
                case msg of
                    Card.Selected dir ->
                        Selected <| dir

                    Card.Over dir ->
                        MouseOver dir
            )
    ]
