module DungeonSokoban.Main exposing (..)

import Browser
import Browser.Events as Events
import Direction exposing (Direction(..))
import DungeonSokoban.Data.Game as Game exposing (Game)
import DungeonSokoban.View.Game as Game
import Html.Attributes as Attr
import Json.Decode as Decode
import Layout
import Process
import Random exposing (Generator, Seed)
import Swiper exposing (SwipeEvent, SwipingState)
import Task


type alias Model =
    { game : Maybe Game
    , swipingState : SwipingState
    , seed : Seed
    }


type Msg
    = UpdateModel (Model -> Model)
    | HasSwiped SwipeEvent
    | MoveDir (Maybe Direction)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


newGame : Seed -> Model -> Model
newGame seed model =
    Random.step (Game.new 1) seed
        |> (\( game, newSeed ) ->
                { model
                    | game = Just game
                    , seed = newSeed
                }
           )


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Nothing
      , swipingState = Swiper.initialSwipingState
      , seed = Random.initialSeed 42
      }
    , Random.generate (\seed -> UpdateModel (newGame seed)) Random.independentSeed
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateModel fun ->
            ( fun model, Cmd.none )

        HasSwiped evt ->
            let
                ( _, swipedLeft ) =
                    Swiper.hasSwipedLeft evt model.swipingState

                ( _, swipedRight ) =
                    Swiper.hasSwipedRight evt model.swipingState

                ( _, swipedUp ) =
                    Swiper.hasSwipedUp evt model.swipingState

                ( newState, swipedDown ) =
                    Swiper.hasSwipedDown evt model.swipingState

                dir =
                    if swipedLeft then
                        Just Left

                    else if swipedRight then
                        Just Right

                    else if swipedUp then
                        Just Up

                    else if swipedDown then
                        Just Down

                    else
                        Nothing
            in
            { model | swipingState = newState }
                |> update (MoveDir dir)

        MoveDir maybeDir ->
            case maybeDir of
                Just dir ->
                    let
                        maybeGame =
                            model.game
                                |> Maybe.map (\game -> Game.movePlayer dir game)
                    in
                    ( { model | game = maybeGame }
                    , Process.sleep 100
                        |> Task.perform
                            (\() ->
                                UpdateModel
                                    (\newModel ->
                                        newModel.game
                                            |> Maybe.map
                                                (\game ->
                                                    game
                                                        |> Game.update dir
                                                        |> Random.map (\g -> { newModel | game = Just g })
                                                        |> randomUpdateModel newModel.seed
                                                )
                                            |> Maybe.withDefault newModel
                                    )
                            )
                    )

                Nothing ->
                    ( model, Cmd.none )


randomUpdateModel : Seed -> Generator Model -> Model
randomUpdateModel oldSeed generator =
    let
        ( newModel, seed ) =
            Random.step generator oldSeed
    in
    { newModel | seed = seed }


view : Model -> Browser.Document Msg
view model =
    { title = "Dungeon Sokoban"
    , body =
        model.game
            |> Maybe.map Game.view
            |> Maybe.withDefault Layout.none
            |> Layout.el
                ([ Attr.style "position" "absolute"
                 , Attr.style "left" "50%"
                 , Attr.style "top" "50%"
                 , Attr.style "transform" "translate(-50%, -50%)"
                 ]
                    ++ Swiper.onSwipeEvents HasSwiped
                )
            |> List.singleton
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Decode.field "key" Decode.string
        |> Decode.map
            (\string ->
                case string of
                    "W" ->
                        Just Up

                    "D" ->
                        Just Right

                    "S" ->
                        Just Down

                    "A" ->
                        Just Left

                    "w" ->
                        Just Up

                    "d" ->
                        Just Right

                    "s" ->
                        Just Down

                    "a" ->
                        Just Left

                    "ArrowLeft" ->
                        Just Left

                    "ArrowRight" ->
                        Just Right

                    "ArrowDown" ->
                        Just Down

                    "ArrowUp" ->
                        Just Up

                    _ ->
                        Nothing
            )
        |> Decode.map MoveDir
        |> Events.onKeyDown
