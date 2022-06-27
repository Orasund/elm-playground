module Main exposing (main)

import Browser exposing (Document)
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random exposing (Seed)
import Tile exposing (Tile(..))


type alias Model =
    { game : Game
    , seed : Seed
    }


type Msg
    = GotSeed Seed
    | TileClicked ( Int, Int )


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        ( game, _ ) =
            seed
                |> Random.step Game.new
    in
    ( { game = game
      , seed = seed
      }
    , Random.generate GotSeed Random.independentSeed
    )


view : Model -> Document Msg
view model =
    { title = "Bug Sweeper"
    , body =
        List.range 0 (Config.gridSize - 1)
            |> List.map
                (\y ->
                    List.range 0 (Config.gridSize - 1)
                        |> List.map
                            (\x ->
                                (case model.game.grid |> Dict.get ( x, y ) of
                                    Just Stone ->
                                        case Game.nearestBug ( x, y ) model.game of
                                            0 ->
                                                "0️⃣"

                                            1 ->
                                                "1️⃣"

                                            2 ->
                                                "2️⃣"

                                            3 ->
                                                "3️⃣"

                                            4 ->
                                                "4️⃣"

                                            5 ->
                                                "5️⃣"

                                            6 ->
                                                "6️⃣"

                                            7 ->
                                                "7️⃣"

                                            8 ->
                                                "8️⃣"

                                            9 ->
                                                "9️⃣"

                                            10 ->
                                                "🔟"

                                            _ ->
                                                "❓"

                                    Just (Bug { visible }) ->
                                        if visible then
                                            { visible = visible }
                                                |> Bug
                                                |> Tile.toString

                                        else
                                            "❓"

                                    Just Leaf ->
                                        Tile.toString Leaf

                                    _ ->
                                        "❓"
                                )
                                    |> Tuple.pair ( x, y )
                            )
                        |> List.map
                            (\( pos, string ) ->
                                string
                                    |> Html.text
                                    |> List.singleton
                                    |> Html.a
                                        [ Html.Attributes.href "#"
                                        , Html.Events.onClick (TileClicked pos)
                                        ]
                            )
                        |> Html.div []
                )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            seed
                |> Random.step (Game.new |> Random.map Game.removeLeafs)
                |> (\( game, _ ) -> ( { model | game = game }, Cmd.none ))

        TileClicked pos ->
            model.seed
                |> Random.step
                    (model.game
                        |> Game.reveal pos
                        |> Game.moveBug
                        |> Random.map Game.removeLeafs
                    )
                |> (\( game, _ ) -> ( { model | game = game }, Cmd.none ))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
