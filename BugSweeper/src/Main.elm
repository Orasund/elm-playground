module Main exposing (main)

import Browser exposing (Document)
import Config
import Dict
import Game exposing (Game)
import Html
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
        [ List.range 0 (Config.gridSize - 1)
            |> List.map
                (\y ->
                    List.range 0 (Config.gridSize - 1)
                        |> List.map
                            (\x ->
                                (case model.game.grid |> Dict.get ( x, y ) of
                                    Just (Bug { visible }) ->
                                        if visible then
                                            { visible = visible }
                                                |> Bug
                                                |> Tile.toString

                                        else
                                            ""

                                    Just tile ->
                                        Tile.toString tile

                                    Nothing ->
                                        ""
                                )
                                    |> Tuple.pair ( x, y )
                            )
                        |> List.map
                            (\( pos, string ) ->
                                string
                                    |> Html.text
                                    |> List.singleton
                                    |> Html.div
                                        [ Html.Attributes.style "display" "flex"
                                        , Html.Attributes.style "justify-content" "center"
                                        , Html.Attributes.style "align-items" "center"
                                        , Html.Attributes.style "height" "40px"
                                        , Html.Attributes.style "width" "40px"
                                        , Html.Attributes.style "border" "solid 1px gray"
                                        ]
                                    |> List.singleton
                                    |> Html.a
                                        [ Html.Attributes.href "#"
                                        , Html.Events.onClick (TileClicked pos)
                                        , Html.Attributes.style "font-size" "30px"
                                        , Html.Attributes.style "text-decoration" "none"
                                        ]
                            )
                        |> Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "row"
                            ]
                )
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                ]
        , "Remaining Bugs: "
            ++ (model.game.grid
                    |> Dict.filter
                        (\_ tile ->
                            case tile of
                                Bug { visible } ->
                                    not visible

                                _ ->
                                    False
                        )
                    |> Dict.size
                    |> String.fromInt
               )
            |> Html.text
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            seed
                |> Random.step (Game.new |> Random.map Game.removeLeafs)
                |> (\( game, newSeed ) -> ( { model | game = game, seed = newSeed }, Cmd.none ))

        TileClicked pos ->
            model.seed
                |> Random.step
                    (model.game
                        |> Game.removeCatchedBugs
                        |> Game.reveal pos
                        |> Game.moveBug
                        |> Random.map Game.removeLeafs
                    )
                |> (\( game, newSeed ) -> ( { model | game = game, seed = newSeed }, Cmd.none ))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
