module AsteroidMiner.Page.Game exposing (Model, Msg, areas, init, subscriptions, update)

import Action exposing (Action)
import AsteroidMiner.Data exposing (size)
import AsteroidMiner.Data.Map as Map exposing (GroundType(..), Map)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.View.RunningGame as RunningGame exposing (Status(..))
import Grid.Bordered as Grid
import PixelEngine exposing (Area)
import Random exposing (Seed)


type alias Model =
    RunningGame.Model


type alias GameAction =
    Action Model Never Never ()


init : Seed -> ( Model, Cmd msg )
init seed =
    ( RunningGame.init { map = Map.init, seed = seed, winCondition = 4000 }, Cmd.none )


type alias Msg =
    RunningGame.Msg


subscriptions : Model -> Sub Msg
subscriptions =
    RunningGame.subscriptions


update : Msg -> Model -> GameAction
update msg =
    RunningGame.update msg
        >> (\model ->
                case model.status of
                    Running ->
                        Action.updating ( model, Cmd.none )

                    _ ->
                        Action.exiting
           )


areas : Model -> List (Area Msg)
areas =
    RunningGame.areas []
