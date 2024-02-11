module Main exposing (main)

import Browser exposing (Document)
import BugSpecies exposing (BugSpecies)
import Config
import Dict
import Game exposing (Game)
import Game.Update
import Html
import Html.Attributes
import Html.Events
import Html.Style
import Layout
import Process
import Random exposing (Seed)
import Set.Any as AnySet exposing (AnySet)
import Task
import Tile exposing (Tile(..))


type alias Model =
    { game : Game
    , seed : Seed
    , viewCollection : Bool
    }


type Msg
    = NewGame { seed : Seed, collectedBugs : AnySet String BugSpecies, level : Int }
    | TileClicked ( Int, Int )
    | ToggleViewCollection


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        ( game, _ ) =
            seed
                |> Random.step (Game.new 1 (AnySet.empty BugSpecies.toString))
    in
    ( { game = game
      , seed = seed
      , viewCollection = False
      }
    , Random.independentSeed
        |> Random.map
            (\s ->
                { seed = s
                , collectedBugs = AnySet.empty BugSpecies.toString
                , level = 1
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
                [ Html.Attributes.style "background-color" "rgba(255,255,255,0.2)"
                , Html.Attributes.style "height" "48px"
                , Html.Attributes.style "border-radius" "10000px"
                , Html.Style.alignItemsCenter
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
                                               , Html.Attributes.style "background-color" "white"
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
                        |> Layout.row [ Layout.noWrap, Layout.gap 8 ]
                )
            |> Layout.column (Layout.centered ++ [ Layout.gap 8 ])
        , [ "Turns remaining: " |> Html.text
          , (Config.maxTurns - model.game.turn |> String.fromInt)
                |> Html.text
                |> Layout.el
                    (Layout.centered
                        ++ [ Html.Attributes.style "font-size" "28px"
                           , Html.Attributes.style "background-color" "rgba(255,255,255,0.2)"
                           , Html.Attributes.style "height" "48px"
                           , Html.Attributes.style "width" "48px"
                           , Html.Attributes.style "border-radius" "10000px"
                           ]
                    )
          ]
            |> Layout.row
                [ Html.Attributes.style "justify-content" "flex-end"
                , Html.Style.alignItemsCenter
                , Html.Attributes.style "height" "48px"
                , Layout.gap 16
                ]
        , Layout.none
            |> Layout.container
                (Layout.centered
                    ++ [ Html.Attributes.style "background-color" "rgb(70, 109, 34,0.5)"
                       , Html.Attributes.style "backdrop-filter" "blur(2px)"
                       , Html.Events.onClick ToggleViewCollection
                       ]
                    ++ (if model.viewCollection then
                            []

                        else
                            [ Html.Attributes.style "display" "none"
                            ]
                       )
                )
        , (if model.viewCollection then
            [ "Your collection:"
                |> Html.text
                |> Layout.el
                    [ Html.Attributes.style "padding" "8px 16px"
                    , Html.Attributes.style "height" "48px"
                    , Html.Style.alignItemsCenter
                    ]
            , (BugSpecies.list
                |> List.map
                    (\species ->
                        if model.game.collectedBugs |> AnySet.member species then
                            BugSpecies.toString species

                        else
                            "❓"
                    )
                |> List.map
                    (\string ->
                        string
                            |> Html.text
                            |> Layout.el
                                (Layout.centered
                                    ++ [ Html.Attributes.style "border-radius" "32px"
                                       , Html.Attributes.style "background-color" "rgba(0,0,0,0.1)"
                                       , Html.Attributes.style "height" "48px"
                                       , Html.Attributes.style "width" "48px"
                                       ]
                                )
                    )
              )
                |> Layout.row
                    [ Html.Attributes.style "padding" "16px"
                    , Html.Attributes.style "font-size" "20px"
                    , Html.Style.alignItemsCenter
                    , Layout.gap 16
                    ]
            ]

           else
            [ [ "Your collection:"
                    |> Html.text
                    |> Layout.el
                        [ Html.Attributes.style "padding" "8px 16px"
                        , Html.Style.alignItemsCenter
                        ]
              , (model.game.collectedBugs
                    |> AnySet.toList
                    |> List.map BugSpecies.toString
                    |> String.concat
                )
                    |> Html.text
                    |> Layout.el
                        [ Html.Attributes.style "padding" "8px 16px"
                        , Html.Attributes.style "font-size" "20px"
                        , Html.Style.alignItemsCenter
                        ]
              ]
                |> Layout.row
                    [ Html.Attributes.style "height" "48px"
                    , Html.Style.alignItemsCenter
                    , Layout.contentWithSpaceBetween
                    ]
                |> List.singleton
                |> Html.a
                    [ Html.Attributes.href "#"
                    , Html.Events.onClick ToggleViewCollection
                    , Html.Attributes.style "text-decoration" "none"
                    , Html.Attributes.style "color" "black"
                    ]
            ]
          )
            |> Layout.column
                ((if model.viewCollection then
                    [ Html.Attributes.style "height" "250px" ]

                  else
                    [ Html.Attributes.style "height" "48px" ]
                 )
                    ++ [ Html.Attributes.style "position" "fixed"
                       , Html.Attributes.style "bottom" "0px"
                       , Html.Attributes.style "width" "352px"
                       , Html.Attributes.style "background-color" "white"
                       , Html.Attributes.style "border-top-left-radius" "8px"
                       , Html.Attributes.style "border-top-right-radius" "8px"
                       , Html.Attributes.style "transition" "height 0.2s"
                       ]
                )
        ]
            |> Layout.column
                [ Layout.gap 8
                , Html.Attributes.style "height" "100%"
                , Html.Style.justifyContentCenter
                ]
            |> Layout.container
                (Layout.centered
                    ++ [ Html.Attributes.style "background-image" "linear-gradient(#D1884D,#466D22)"
                       ]
                )
            |> List.singleton
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame { seed, collectedBugs, level } ->
            seed
                |> Random.step (Game.new level collectedBugs |> Random.map (Game.removeLeafs ( -1, -1 )))
                |> (\( game, newSeed ) ->
                        ( { game = game
                          , seed = newSeed
                          , viewCollection = False
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
                                , level = model.game.level + 1
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
                            |> Game.Update.moveBugs
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

        ToggleViewCollection ->
            ( { model | viewCollection = not model.viewCollection }, Cmd.none )


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
