module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Color
import Dict exposing (Dict)
import Game exposing (Game, SavedLevel)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Platform.Cmd as Cmd
import Time
import View


type alias Model =
    { game : Maybe Game
    , levels : Dict Int SavedLevel
    , updating : Bool
    , level : Int
    , selected : Maybe ( Int, Int )
    }


type Msg
    = Toggle ( Int, Int )
    | PlaceModule { moduleId : Int, rotation : Int }
    | Unselect
    | UpdateGrid
    | NextLevel
    | SelectLevel Int


init : () -> ( Model, Cmd Msg )
init () =
    let
        level =
            1
    in
    ( { game = Game.Generate.fromId level
      , levels = Dict.empty
      , updating = False
      , level = level
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
                            |> Dict.insert model.level savedGame
                    )
                |> Maybe.withDefault model.levels
    }


loadLevel : Int -> Model -> Model
loadLevel level model =
    let
        grid =
            model.levels
                |> Dict.get level
                |> Maybe.map Game.fromSave
                |> Maybe.map Just
                |> Maybe.withDefault (Game.Generate.fromId level)
    in
    { model
        | game = grid
        , level = level
    }


generateLevel : Int -> Model -> Model
generateLevel level model =
    { model
        | game = Game.Generate.fromId level
        , level = level
    }


view : Model -> Html Msg
view model =
    [ [ "Stage " ++ String.fromInt model.level |> View.title
      , model.game
            |> Maybe.map
                (\game ->
                    game
                        |> View.grid []
                            { levels = model.levels
                            , onToggle = Toggle
                            }
                )
            |> Maybe.withDefault View.gameWon
      , [ "Edit Stages" |> View.cardTitle
        , model.levels
            |> View.savedLevels SelectLevel
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
        , View.primaryButton NextLevel
            "Next Level"
        ]
            |> Just

       else if model.selected /= Nothing then
        [ "Select a tile you want to place"
            |> View.cardTitle
        , model.levels
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
                                    |> View.tile2 { level = model.game |> Maybe.map .level |> Maybe.withDefault 1 } model.levels
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
                        (\{ stage, level } ->
                            case level of
                                1 ->
                                    stage.grid
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
                                                        { stage = { stage | grid = grid }
                                                        , level = level
                                                        }
                                                            |> Just
                                                }
                                           )

                                2 ->
                                    case stage.grid |> Dict.get ( x, y ) of
                                        Just (ConnectionCell _) ->
                                            stage.grid
                                                |> Dict.remove ( x, y )
                                                |> (\grid ->
                                                        { model
                                                            | game =
                                                                { stage = { stage | grid = grid }
                                                                , level = level
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
                    (\{ stage, level } ->
                        case level of
                            1 ->
                                model

                            2 ->
                                stage.grid
                                    |> Dict.insert (model.selected |> Maybe.withDefault ( 0, 0 ))
                                        ({ moduleId = moduleId
                                         , rotation = rotation
                                         , sendsTo = Dict.empty
                                         }
                                            |> ConnectionCell
                                        )
                                    |> (\grid ->
                                            { model
                                                | game = Just { level = level, stage = { stage | grid = grid } }
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
                                    |> Game.update model.levels
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

        NextLevel ->
            ( model
                |> saveLevel
                |> loadLevel (model.level + 1)
            , Cmd.none
            )

        Unselect ->
            ( { model | selected = Nothing }, Cmd.none )

        SelectLevel level ->
            ( model |> generateLevel level
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
