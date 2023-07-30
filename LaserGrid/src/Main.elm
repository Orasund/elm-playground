module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Color
import Dict exposing (Dict)
import Game exposing (Game, SavedStage)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Platform.Cmd as Cmd
import Time
import View


type alias Model =
    { game : Maybe Game
    , levels : Dict Int (Dict Int SavedStage)
    , updating : Bool
    , stage : Int
    , level : Int
    , selected : Maybe ( Int, Int )
    }


type Msg
    = Toggle ( Int, Int )
    | PlaceModule { moduleId : Int, rotation : Int }
    | Unselect
    | UpdateGrid
    | NextStage
    | LoadStage { level : Int, stage : Int }


init : () -> ( Model, Cmd Msg )
init () =
    let
        level =
            1

        stage =
            1
    in
    ( { game = Game.Generate.new { level = level, stage = stage }
      , levels = Dict.empty
      , updating = False
      , level = level
      , stage = 1
      , selected = Nothing
      }
    , Cmd.none
    )


saveLevel : Model -> Model
saveLevel model =
    { model
        | levels =
            model.game
                |> Maybe.andThen Game.toSave
                |> Maybe.map
                    (\savedGame ->
                        model.levels
                            |> Dict.update model.level
                                (\maybe ->
                                    maybe
                                        |> Maybe.withDefault Dict.empty
                                        |> Dict.insert model.stage savedGame
                                        |> Just
                                )
                    )
                |> Maybe.withDefault model.levels
    }


loadStage : { stage : Int, level : Int } -> Model -> Maybe Model
loadStage args model =
    model.levels
        |> Dict.get args.level
        |> Maybe.withDefault Dict.empty
        |> Dict.get args.stage
        |> Maybe.map Game.fromSave
        |> Maybe.map Just
        |> Maybe.withDefault (Game.Generate.new args)
        |> Maybe.map
            (\grid ->
                { model
                    | game = Just grid
                    , level = args.level
                    , stage = args.stage
                }
            )


generateStage : { stage : Int, level : Int } -> Model -> Model
generateStage args model =
    { model
        | game = Game.Generate.new args
        , level = args.level
        , stage = args.stage
    }


view : Model -> Html Msg
view model =
    [ [ "Level " ++ String.fromInt model.level ++ " - " ++ String.fromInt model.stage |> View.title
      , model.game
            |> Maybe.map
                (\game ->
                    game
                        |> View.grid []
                            { levels =
                                model.levels
                                    |> Dict.get (model.level - 1)
                                    |> Maybe.withDefault Dict.empty
                            , onToggle = Toggle
                            }
                )
            |> Maybe.withDefault View.gameWon
      , [ "Edit Stages" |> View.cardTitle
        , model.levels
            |> Dict.get (model.level - 1)
            |> Maybe.withDefault Dict.empty
            |> View.savedLevels { level = model.level - 1 }
                (\stage -> LoadStage { level = model.level - 1, stage = stage })
        ]
            |> View.card [ Layout.gap 16 ]
      ]
        |> Layout.column [ Layout.gap 16 ]
    , (if
        (model.updating == False)
            && (model.game
                    |> Maybe.map Game.isSolved
                    |> Maybe.withDefault False
               )
       then
        [ "Stage Complete"
            |> View.cardTitle
        , View.primaryButton NextStage
            "Next Level"
        ]
            |> Just

       else if model.selected /= Nothing then
        [ "Select a tile you want to place"
            |> View.cardTitle
        , model.levels
            |> Dict.get (model.level - 1)
            |> Maybe.withDefault Dict.empty
            |> Dict.keys
            |> List.map
                (\id ->
                    List.range 0 3
                        |> List.map
                            (\rotate ->
                                { moduleId = id
                                , rotation = rotate
                                , sendsTo = Dict.empty
                                }
                                    |> ConnectionCell
                                    |> View.tile2 { level = model.game |> Maybe.map .level |> Maybe.withDefault 1 }
                                        (model.levels |> Dict.get (model.level - 1) |> Maybe.withDefault Dict.empty)
                                    |> Layout.el
                                        (Layout.asButton
                                            { label = "Level " ++ String.fromInt id ++ "(" ++ String.fromInt rotate ++ ")"
                                            , onPress = Just (PlaceModule { moduleId = id, rotation = rotate })
                                            }
                                        )
                            )
                        |> Layout.row [ Layout.gap 8 ]
                )
            |> Layout.column [ Layout.gap 8 ]
        , View.button Unselect
            "Cancel"
        ]
            |> Just

       else
        Nothing
      )
        |> Maybe.map
            (\html ->
                html
                    |> View.card
                        Layout.centered
                    |> Layout.el
                        (Layout.centered
                            ++ [ Html.Attributes.style "width" "100%"
                               , Html.Attributes.style "height" "100%"
                               , Html.Attributes.style "position" "absolute"
                               ]
                        )
            )
        |> Maybe.withDefault Layout.none
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=device-width, initial-scale=1"
        ]
        []
    , View.stylesheet
    ]
        |> Html.div
            (Layout.centered
                ++ [ Layout.asEl
                   , Html.Attributes.style "position" "relative"
                   , Html.Attributes.style "color" Color.fontColor
                   , Html.Attributes.style "background" (Color.background (model.game |> Maybe.map .level |> Maybe.withDefault 1))
                   , Html.Attributes.style "font-family" "sans-serif"
                   , Html.Attributes.style "height" "100%"
                   ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ( x, y ) ->
            ( (if x >= 0 && x <= 3 && y >= 0 && y <= 3 then
                model.game
                    |> Maybe.map
                        (\game ->
                            case game.level of
                                1 ->
                                    game.stage.grid
                                        |> Dict.update ( x, y )
                                            (\maybe ->
                                                case maybe of
                                                    Just (ConnectionCell _) ->
                                                        Nothing

                                                    Nothing ->
                                                        Just
                                                            (Dict.empty
                                                                |> Cell.connectionLevel1
                                                                |> ConnectionCell
                                                            )

                                                    _ ->
                                                        maybe
                                            )
                                        |> (\grid ->
                                                { model
                                                    | game =
                                                        { game
                                                            | stage = game.stage |> (\stage -> { stage | grid = grid })
                                                        }
                                                            |> Just
                                                }
                                           )

                                2 ->
                                    case game.stage.grid |> Dict.get ( x, y ) of
                                        Just (ConnectionCell _) ->
                                            game.stage.grid
                                                |> Dict.remove ( x, y )
                                                |> (\grid ->
                                                        { model
                                                            | game =
                                                                { game
                                                                    | stage = game.stage |> (\stage -> { stage | grid = grid })
                                                                }
                                                                    |> Just
                                                        }
                                                   )

                                        Nothing ->
                                            { model | selected = Just ( x, y ) }

                                        _ ->
                                            model

                                _ ->
                                    Debug.todo "recursive Updates"
                        )
                    |> Maybe.withDefault model

               else
                model
              )
                |> (\m -> { m | updating = True })
            , Cmd.none
            )

        PlaceModule { moduleId, rotation } ->
            ( model.game
                |> Maybe.map
                    (\game ->
                        case game.level of
                            1 ->
                                model

                            2 ->
                                game.stage.grid
                                    |> Dict.insert (model.selected |> Maybe.withDefault ( 0, 0 ))
                                        ({ moduleId = moduleId
                                         , rotation = rotation
                                         , sendsTo = Dict.empty
                                         }
                                            |> ConnectionCell
                                        )
                                    |> (\grid ->
                                            { model
                                                | game = Just { game | stage = game.stage |> (\stage -> { stage | grid = grid }) }
                                                , updating = True
                                                , selected = Nothing
                                            }
                                       )

                            _ ->
                                Debug.todo "recursive placing modules"
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        UpdateGrid ->
            let
                ( newGrid, updating ) =
                    model.game
                        |> Maybe.map
                            (\game ->
                                game
                                    |> Game.update (model.levels |> Dict.get (model.level - 1) |> Maybe.withDefault Dict.empty)
                                    |> Tuple.mapFirst Just
                            )
                        |> Maybe.withDefault ( Nothing, False )
            in
            ( { model
                | game = newGrid
                , updating = updating
              }
            , Cmd.none
            )

        NextStage ->
            ( model
                |> saveLevel
                |> (\m ->
                        case
                            m |> loadStage { level = m.level, stage = m.stage + 1 }
                        of
                            Just a ->
                                a

                            Nothing ->
                                m
                                    |> loadStage { level = m.level + 1, stage = 1 }
                                    |> Maybe.withDefault { m | game = Nothing }
                   )
            , Cmd.none
            )

        Unselect ->
            ( { model | selected = Nothing }, Cmd.none )

        LoadStage level ->
            ( model |> generateStage level
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.updating then
        Time.every 75 (\_ -> UpdateGrid)

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
