module Main exposing (main)

import Browser exposing (Document)
import BugSpecies exposing (BugSpecies)
import Collection exposing (Collection)
import Color
import Dict
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
    = Collection (Maybe BugSpecies)
    | Summary


type alias Model =
    { game : Game
    , seed : Seed
    , oldCollection : Collection
    , overlay : Maybe Overlay
    }


type Msg
    = NewGame
        { seed : Seed
        , level : Int
        }
    | TileClicked ( Int, Int )
    | SelectBugSpecies BugSpecies
    | OpenOverlay Overlay
    | CloseOverlay


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        ( game, _ ) =
            seed
                |> Random.step (Game.Generate.new 1)
    in
    ( { game = game
      , seed = seed
      , overlay = Nothing
      , oldCollection = Collection.empty
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
                        BugSpecies.toString bug

                    else
                        "❓"
                )
            |> List.sort
            |> String.concat
          )
            |> Layout.text [ Html.Attributes.style "filter" "brightness(0)" ]
            |> Layout.el
                [ Html.Attributes.style "padding" "8px 16px"
                , Html.Attributes.style "background-color" Color.lightTransparent
                , Html.Attributes.style "height" "48px"
                , Html.Attributes.style "width" "fit-content"
                , Html.Attributes.style "min-width" "48px"
                , Html.Attributes.style "border-radius" "10000px"
                , Html.Attributes.style "font-size" "20px"
                , Html.Attributes.class "emoji-color-font"
                , Html.Style.boxSizingBorderBox
                , Html.Style.alignItemsCenter
                ]
        , View.Game.board { onSelect = TileClicked } model.game
        , (List.repeat model.game.remainingGuesses "❌" |> String.concat)
            |> Html.text
            |> Layout.el
                (Layout.centered
                    ++ [ Html.Attributes.style "background-color" Color.lightTransparent
                       , Html.Attributes.style "height" "48px"
                       , Html.Attributes.style "min-width" "48px"
                       , Html.Attributes.style "border-radius" "10000px"
                       , Html.Attributes.style "padding" "8px 16px"
                       , Html.Attributes.style "font-size" "20px"
                       , Html.Style.boxSizingBorderBox
                       , Html.Attributes.class "emoji-color-font"
                       ]
                )
            |> Layout.el
                [ Html.Style.justifyContentCenter
                ]
        , Layout.none
            |> Layout.container
                (Layout.centered
                    ++ [ Html.Attributes.style "background-color" "rgb(70, 109, 34,0.5)"
                       , Html.Attributes.style "backdrop-filter" "blur(2px)"
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
                                [ Html.Attributes.style "display" "none"
                                ]
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
                        [ View.Collection.closedCollection []
                            { onOpen = OpenOverlay (Collection Nothing)
                            }
                            model.oldCollection
                        ]
               )
            |> Layout.column
                [ Layout.gap 8
                , Html.Attributes.style "height" "100%"
                , Html.Style.justifyContentCenter
                ]
            |> Layout.container
                (Layout.centered
                    ++ [ Html.Attributes.style "background-image"
                            ("linear-gradient(#D1884D," ++ Color.primary ++ ")")
                       ]
                )
            |> List.singleton
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame { seed, level } ->
            seed
                |> Random.step (Game.Generate.new level)
                |> (\( game, newSeed ) ->
                        ( { game = game
                          , seed = newSeed
                          , overlay = Nothing
                          , oldCollection = model.oldCollection |> Collection.add model.game.collected
                          }
                        , Cmd.none
                        )
                   )

        TileClicked pos ->
            if Game.isOver model.game then
                ( model, Cmd.none )

            else
                model.game
                    |> Game.reveal pos
                    |> (\game ->
                            ( { model | game = game }
                            , if Game.isOver game then
                                Process.sleep 1000
                                    |> Task.perform (\() -> OpenOverlay Summary)

                              else
                                Cmd.none
                            )
                       )

        SelectBugSpecies bug ->
            ( { model | overlay = Just (Collection (Just bug)) }, Cmd.none )

        OpenOverlay overlay ->
            ( { model | overlay = Just overlay }, Cmd.none )

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
