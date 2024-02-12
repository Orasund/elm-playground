module Main exposing (main)

import Browser exposing (Document)
import BugSpecies exposing (BugSpecies)
import Config
import Dict
import Game exposing (Game)
import Game.Generate
import Html
import Html.Attributes
import Html.Events
import Html.Style
import Layout
import Object exposing (Object(..))
import Process
import Random exposing (Seed)
import Set
import Set.Any as AnySet exposing (AnySet)
import Task
import View.Collection


type Overlay
    = Collection (Maybe BugSpecies)


type alias Model =
    { game : Game
    , seed : Seed
    , overlay : Maybe Overlay
    }


type Msg
    = NewGame { seed : Seed, collectedBugs : AnySet String BugSpecies, level : Int }
    | TileClicked ( Int, Int )
    | SelectBugSpecies BugSpecies
    | OpenCollection
    | CloseOverlay


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        ( game, _ ) =
            seed
                |> Random.step (Game.Generate.new 1 (AnySet.empty BugSpecies.toString))
    in
    ( { game = game
      , seed = seed
      , overlay = Nothing
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
                (\( pos, species ) ->
                    if Set.member pos model.game.revealed then
                        Nothing

                    else
                        Just species
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
                                (if Set.member ( x, y ) model.game.revealed then
                                    case model.game.grid |> Dict.get ( x, y ) of
                                        Just tile ->
                                            Object.toString tile

                                        Nothing ->
                                            case model.game.bugs |> Dict.get ( x, y ) of
                                                Just species ->
                                                    BugSpecies.toString species

                                                Nothing ->
                                                    "❌"

                                 else
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
        , [ "Remaining guesses: " |> Html.text
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
                       ]
                    ++ (if model.overlay == Nothing then
                            [ Html.Attributes.style "display" "none"
                            ]

                        else
                            [ Html.Events.onClick CloseOverlay ]
                       )
                )
        ]
            ++ (case model.overlay of
                    Just (Collection maybeSelected) ->
                        [ maybeSelected
                            |> Maybe.map View.Collection.detailCard
                            |> Maybe.withDefault (Html.text "")
                        , View.Collection.openCollection []
                            { selected = maybeSelected
                            , onSelect = SelectBugSpecies
                            }
                            model.game.collectedBugs
                        ]

                    Nothing ->
                        [ View.Collection.closedCollection []
                            { onOpen = OpenCollection
                            }
                            model.game.collectedBugs
                        ]
               )
            |> Layout.column
                [ Layout.gap 8
                , Html.Attributes.style "height" "100%"
                , Html.Style.justifyContentCenter
                ]
            |> Layout.container
                (Layout.centered
                    ++ [ Html.Attributes.style "background-image" "linear-gradient(#D1884D,#6a9047)"
                       ]
                )
            |> List.singleton
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame { seed, collectedBugs, level } ->
            seed
                |> Random.step (Game.Generate.new level collectedBugs)
                |> (\( game, newSeed ) ->
                        ( { game = game
                          , seed = newSeed
                          , overlay = Nothing
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
                model.game
                    |> Game.reveal pos
                    |> (\game -> ( { model | game = game }, Cmd.none ))

        SelectBugSpecies bug ->
            ( { model | overlay = Just (Collection (Just bug)) }, Cmd.none )

        OpenCollection ->
            ( { model | overlay = Just (Collection Nothing) }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = Nothing }, Cmd.none )


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
