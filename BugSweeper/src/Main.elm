module Main exposing (main)

import Browser exposing (Document)
import Bug exposing (Bug)
import Collection exposing (Collection, Variant(..))
import Color
import Dict
import Firework exposing (Firework)
import Game exposing (Game, Tile(..))
import Game.Generate
import Html
import Html.Attributes
import Html.Style
import Layout
import Object exposing (Object(..))
import Process
import Random exposing (Seed)
import Task
import View.Collection
import View.Game
import View.Summary


type Overlay
    = Collection (Maybe ( Bug, Variant ))
    | Summary


type alias Model =
    { game : Game
    , seed : Seed
    , oldCollection : Collection
    , overlay : Maybe Overlay
    , firework : Firework
    }


type Msg
    = NewGame
        { seed : Seed
        , level : Int
        }
    | TileClicked ( Int, Int )
    | SelectBugSpecies ( Bug, Variant )
    | OpenOverlay Overlay
    | FireworkRelated Firework.Msg
    | CloseOverlay


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        collection =
            Collection.empty

        ( game, _ ) =
            seed
                |> Random.step (Game.Generate.new 1 collection)
    in
    ( { game = game
      , seed = seed
      , overlay = Nothing
      , oldCollection = collection
      , firework = Firework.init seed
      }
    , Random.independentSeed
        |> Random.map
            (\s ->
                { seed = s
                , level = 1
                }
            )
        |> Random.generate NewGame
    )


view : Model -> Document Msg
view model =
    { title = "Bug Sweeper"
    , body =
        [ (model.game.tiles
            |> Dict.toList
            |> List.filterMap
                (\( pos, tile ) ->
                    case tile of
                        BugTile bug ->
                            if Dict.member pos model.game.revealed then
                                Nothing

                            else
                                Just bug

                        _ ->
                            Nothing
                )
            |> List.map
                (\bug ->
                    if
                        model.oldCollection
                            |> Collection.member bug
                    then
                        Bug.toString bug

                    else
                        "❓"
                )
            |> List.sort
            |> String.concat
          )
            |> Layout.divText [ Html.Style.filter "brightness(0)" ]
            |> Layout.divWrapper
                [ Html.Style.displayFlex
                , Html.Style.padding "8px 16px"
                , Html.Style.backgroundColor Color.lightTransparent
                , Html.Style.heightPx 48
                , Html.Style.widthFitContent
                , Html.Style.minWidthPx 48
                , Html.Style.borderRadius "10000px"
                , Html.Style.fontSizePx 20
                , Html.Style.boxSizingBorderBox
                , Html.Style.alignItemsCenter
                , Html.Attributes.class "emoji-color-font"
                ]
        , View.Game.board { onSelect = TileClicked } model.game
        , (List.repeat model.game.remainingGuesses "❌" |> String.concat)
            |> Html.text
            |> Layout.divWrapper
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.backgroundColor Color.lightTransparent
                , Html.Style.heightPx 48
                , Html.Style.minWidthPx 48
                , Html.Style.borderRadius "10000px"
                , Html.Style.padding "8px 16px"
                , Html.Style.fontSizePx 20
                , Html.Style.boxSizingBorderBox
                , Html.Attributes.class "emoji-color-font"
                ]
            |> Layout.divWrapper
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                ]
        , Layout.none
            |> Layout.divWrapper
                ([ Html.Style.displayFlex
                 , Html.Style.positionFixed
                 , Html.Style.topPx 0
                 , Html.Style.leftPx 0
                 , Html.Style.width "100%"
                 , Html.Style.height "100%"
                 , Html.Style.justifyContentCenter
                 , Html.Style.alignItemsCenter
                 , Html.Style.backgroundColor "rgb(70, 109, 34,0.5)"
                 , Html.Style.backdropFilter "blur(2px)"
                 ]
                    ++ (case model.overlay of
                            Just (Collection _) ->
                                Layout.asButton
                                    { label = "Close Overlay"
                                    , onPress = Just CloseOverlay
                                    }

                            Just Summary ->
                                Layout.asButton
                                    { label = "Continue"
                                    , onPress =
                                        NewGame
                                            { seed = model.seed
                                            , level = model.game.level + 1
                                            }
                                            |> Just
                                    }

                            Nothing ->
                                [ Html.Style.displayNone
                                ]
                       )
                )
        ]
            ++ (case model.overlay of
                    Just (Collection maybeSelected) ->
                        [ maybeSelected
                            |> Maybe.map
                                (\( bug, variant ) ->
                                    View.Collection.detailCard
                                        { bug = bug
                                        , variant = variant
                                        , caught = Collection.count bug model.oldCollection
                                        }
                                )
                            |> Maybe.withDefault (Html.text "")
                        , View.Collection.openCollection []
                            { onSelect = SelectBugSpecies
                            }
                            model.oldCollection
                        ]

                    Just Summary ->
                        [ View.Summary.toHtml
                            { tiles = model.game.tiles
                            , revealed = model.game.revealed
                            , oldCollection = model.oldCollection
                            }
                        ]

                    Nothing ->
                        [ Html.text ""
                        , View.Collection.closedCollection []
                            { onOpen = OpenOverlay (Collection Nothing)
                            }
                            model.oldCollection
                        ]
               )
            ++ [ if Firework.isEmpty model.firework then
                    Layout.none

                 else
                    Firework.view
                        [ Html.Style.positionAbsolute
                        , Html.Style.top "50%"
                        , Html.Style.left "50%"
                        , Html.Style.transform "translate(-50%,-50%)"
                        ]
                        model.firework
               ]
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionColumn
                , Html.Style.gapPx 8
                , Html.Style.height "100%"
                , Html.Style.widthPx 400
                , Html.Style.justifyContentCenter
                ]
            |> Layout.divWrapper
                [ Html.Style.displayFlex
                , Html.Style.height "100%"
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.backgroundImage
                    ("linear-gradient(#ffd3af," ++ Color.primary ++ ")")
                ]
            |> List.singleton
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame { seed, level } ->
            let
                collection =
                    model.oldCollection
                        |> Collection.add model.game.collected
            in
            seed
                |> Random.step (Game.Generate.new level collection)
                |> (\( game, newSeed ) ->
                        ( { game = game
                          , seed = newSeed
                          , overlay = Nothing
                          , oldCollection = collection
                          , firework = Firework.init seed
                          }
                        , Cmd.none
                        )
                   )

        TileClicked pos ->
            if Game.isOver model.game then
                ( model, Cmd.none )

            else
                Random.step (Random.weighted ( 1, Royal ) [ ( 19, Cute ) ]) model.seed
                    |> (\( variant, newSeed ) ->
                            model.game
                                |> Game.reveal pos variant
                                |> (\game ->
                                        ( { model
                                            | game = game
                                            , seed = newSeed
                                            , firework =
                                                if Game.isWon game then
                                                    model.firework
                                                        |> Firework.burst
                                                        |> Firework.burst
                                                        |> Firework.burst

                                                else
                                                    model.firework
                                          }
                                        , if Game.isOver game then
                                            Process.sleep 500
                                                |> Task.perform (\() -> OpenOverlay Summary)

                                          else
                                            Cmd.none
                                        )
                                   )
                       )

        SelectBugSpecies ( bug, variant ) ->
            ( { model | overlay = Just (Collection (Just ( bug, variant ))) }, Cmd.none )

        OpenOverlay overlay ->
            ( { model
                | overlay = Just overlay
              }
            , Cmd.none
            )

        CloseOverlay ->
            ( { model | overlay = Nothing }, Cmd.none )

        FireworkRelated m ->
            ( { model | firework = Firework.update m model.firework }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Firework.sub FireworkRelated model.firework


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
