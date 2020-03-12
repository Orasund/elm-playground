module HeroForge.Page.Playing exposing (Model, Msg, TansitionData, init, subscriptions, update, view)

import Action
import Browser.Events as Events
import Element exposing (Element)
import HeroForge.Data.Game as Game exposing (Direction(..), Game, Msg(..))
import HeroForge.View.Card as Card
import HeroForge.View.Game as Game
import Json.Decode as Decode
import Process
import Random exposing (Seed)
import Task


type alias Model =
    { seed : Seed
    , game : Game
    , selected : Maybe Direction
    , showAnimation : Bool
    , swipPos : Maybe Float
    }


type Msg
    = Pressed (Maybe Direction)
    | Selected Direction
    | MouseOver (Maybe Direction)
    | Swiping (Maybe Float)
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
      , swipPos = Nothing
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

        Swiping maybeX ->
            Action.updating <|
                case ( maybeX, model.swipPos ) of
                    ( Nothing, _ ) ->
                        model.selected
                            |> Maybe.map
                                (\dir ->
                                    ( select dir
                                        |> (\m -> { m | swipPos = Nothing })
                                    , Cmd.batch
                                        [ Process.sleep 10
                                            |> Task.perform (always ActivateAnimation)
                                        ]
                                    )
                                )
                            |> Maybe.withDefault ( model, Cmd.none )

                    ( Just x, Just oldX ) ->
                        if abs (oldX - x) >= 10 then
                            ( { model
                                | selected =
                                    Just <|
                                        if oldX - x > 0 then
                                            Left

                                        else
                                            Right
                                , swipPos = Just x
                              }
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    ( Just x, Nothing ) ->
                        ( { model | swipPos = Just x }, Cmd.none )

        ActivateAnimation ->
            Action.updating
                ( { model | showAnimation = True }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
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

                    Card.Swiping x ->
                        Swiping x
            )
    ]
