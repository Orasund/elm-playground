module Main exposing (main)

import Browser exposing (Document)
import BugSpecies exposing (BugSpecies)
import Config
import Dict
import Game exposing (Game)
import Html
import Html.Attributes
import Html.Events
import Layout
import Material.Elevation as Elevation
import Material.Theme as Theme
import Process
import Random exposing (Seed)
import Set.Any as AnySet exposing (AnySet)
import Task
import Tile exposing (Tile(..))


type alias Model =
    { game : Game
    , seed : Seed
    }


type Msg
    = NewGame { seed : Seed, collectedBugs : AnySet String BugSpecies }
    | TileClicked ( Int, Int )


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        ( game, _ ) =
            seed
                |> Random.step (Game.new (AnySet.empty BugSpecies.toString))
    in
    ( { game = game
      , seed = seed
      }
    , Random.independentSeed
        |> Random.map
            (\s ->
                { seed = s
                , collectedBugs = AnySet.empty BugSpecies.toString
                }
            )
        |> Random.generate NewGame
    )


view : Model -> Document Msg
view model =
    { title = "Bug Sweeper"
    , body =
        [ (model.game.bugs
            |> Dict.toList
            |> List.filterMap
                (\( _, { visible, species } ) ->
                    if not visible then
                        Just species

                    else
                        Nothing
                )
            |> List.map
                (\species ->
                    if
                        model.game.collectedBugs
                            |> AnySet.member species
                    then
                        BugSpecies.toString species

                    else
                        "❓"
                )
            |> List.sort
            |> String.concat
          )
            |> Html.text
            |> Layout.el [ Html.Attributes.style "padding" "8px 16px" ]
            |> Layout.el
                [ Theme.surface
                , Elevation.z1
                , Html.Attributes.style "height" "48px"
                , Html.Attributes.style "border-radius" "8px"
                , Layout.alignCenter
                ]
        , List.range 0 (Config.gridSize - 1)
            |> List.map
                (\y ->
                    List.range 0 (Config.gridSize - 1)
                        |> List.map
                            (\x ->
                                (case model.game.grid |> Dict.get ( x, y ) of
                                    Just tile ->
                                        Tile.toString tile

                                    Nothing ->
                                        case model.game.bugs |> Dict.get ( x, y ) of
                                            Just { visible, species } ->
                                                if not Config.hideBugs || visible then
                                                    BugSpecies.toString species

                                                else
                                                    ""

                                            Nothing ->
                                                ""
                                )
                                    |> Tuple.pair ( x, y )
                            )
                        |> List.map
                            (\( pos, string ) ->
                                string
                                    |> Html.text
                                    |> Layout.el
                                        (Layout.centered
                                            ++ [ Html.Attributes.style "height" "64px"
                                               , Html.Attributes.style "width" "64px"
                                               , Html.Attributes.style "border-radius" "8px"
                                               , Theme.surface
                                               , Elevation.z2
                                               ]
                                        )
                                    |> List.singleton
                                    |> Html.a
                                        [ Html.Attributes.href "#"
                                        , Html.Events.onClick (TileClicked pos)
                                        , Html.Attributes.style "font-size" "30px"
                                        , Html.Attributes.style "text-decoration" "none"
                                        ]
                            )
                        |> Layout.row [ Layout.noWrap, Layout.spacing 8 ]
                )
            |> Layout.column (Layout.centered ++ [ Layout.spacing 8 ])
        , [ "Turns remaining: " |> Html.text
          , (Config.maxTurns - model.game.turn |> String.fromInt)
                |> Html.text
                |> Layout.el [ Html.Attributes.style "font-size" "28px" ]
          ]
            |> Layout.row
                [ Html.Attributes.style "justify-content" "flex-end"
                , Layout.alignCenter
                , Html.Attributes.style "height" "48px"
                , Layout.spacing 16
                ]
        , [ "collected Bugs:"
                |> Html.text
                |> Layout.el
                    [ Html.Attributes.style "padding" "8px 16px"
                    , Layout.alignCenter
                    ]
          , (model.game.collectedBugs
                |> AnySet.toList
                |> List.map BugSpecies.toString
                |> String.concat
            )
                |> Html.text
                |> Layout.el [ Html.Attributes.style "padding" "8px 16px", Html.Attributes.style "font-size" "20px", Layout.alignCenter ]
          ]
            |> Layout.row
                [ Html.Attributes.style "height" "48px"
                , Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "bottom" "0px"
                , Html.Attributes.style "width" "352px"
                , Theme.surface
                , Elevation.z8
                , Html.Attributes.style "border-top-left-radius" "8px"
                , Html.Attributes.style "border-top-right-radius" "8px"
                ]
        ]
            |> Layout.column [ Layout.spacing 8, Html.Attributes.style "height" "100%", Layout.centerContent ]
            |> Layout.container
                (Layout.centered
                    ++ [ Theme.primaryBg
                       ]
                )
            |> List.singleton
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame { seed, collectedBugs } ->
            seed
                |> Random.step (Game.new collectedBugs |> Random.map (Game.removeLeafs ( -1, -1 )))
                |> (\( game, newSeed ) ->
                        ( { game = game
                          , seed = newSeed
                          }
                        , Cmd.none
                        )
                   )

        TileClicked pos ->
            if model.game.turn >= Config.maxTurns then
                ( model
                , Process.sleep Config.gameOverCooldownInMs
                    |> Task.andThen
                        (\() ->
                            Task.succeed
                                { seed = model.seed
                                , collectedBugs = model.game.collectedBugs
                                }
                        )
                    |> Task.perform NewGame
                )

            else
                model.seed
                    |> Random.step
                        (model.game
                            |> Game.removeCatchedBugs
                            |> Game.reveal pos
                            |> Game.moveBugs
                            |> Random.map (Game.removeLeafs pos)
                        )
                    |> (\( game, newSeed ) ->
                            ( { model
                                | game = { game | turn = game.turn + 1 }
                                , seed = newSeed
                              }
                            , Cmd.none
                            )
                       )


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
