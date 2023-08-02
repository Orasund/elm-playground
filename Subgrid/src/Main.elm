module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Color
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level(..))
import Platform.Cmd as Cmd
import Stage exposing (SavedStage)
import Time
import View
import View.TileSelect as TileSelect


type alias Model =
    { game : Maybe Game
    , levels : Dict String (Dict Int SavedStage)
    , updating : Bool
    , stage : Int
    , level : Level
    , selected : Maybe ( Int, Int )
    }


type Msg
    = Toggle ( Int, Int )
    | PlaceModule { moduleId : Int, rotation : Int }
    | Unselect
    | UpdateGrid
    | NextStage
    | LoadStage { level : Level, stage : Int }
    | RemoveTile ( Int, Int )


init : () -> ( Model, Cmd Msg )
init () =
    let
        level =
            Level1

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
                |> Maybe.andThen (Game.toSave model.level)
                |> Maybe.map
                    (\savedGame ->
                        model.levels
                            |> Dict.update (Level.toString model.level)
                                (\maybe ->
                                    maybe
                                        |> Maybe.withDefault Dict.empty
                                        |> Dict.insert model.stage savedGame
                                        |> Just
                                )
                    )
                |> Maybe.withDefault model.levels
    }


loadStage : { stage : Int, level : Level } -> Model -> Maybe Model
loadStage args model =
    model.levels
        |> Dict.get (Level.toString args.level)
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


generateStage : { stage : Int, level : Level } -> Model -> Model
generateStage args model =
    { model
        | game = Game.Generate.new args
        , level = args.level
        , stage = args.stage
    }


view : Model -> Html Msg
view model =
    [ [ "Level " ++ Level.toString model.level ++ " - " ++ String.fromInt model.stage |> View.title
      , model.game
            |> Maybe.map
                (\game ->
                    game
                        |> View.grid []
                            { levels =
                                model.levels
                                    |> Dict.get (model.level |> Level.previous |> Level.toString)
                                    |> Maybe.withDefault Dict.empty
                            , onToggle = Toggle
                            , level = model.level
                            }
                )
            |> Maybe.withDefault View.gameWon
            |> Layout.el
                [ Html.Attributes.style "border-radius" "1rem"
                , Html.Attributes.style "overflow" "hidden"
                ]
      , if model.level == Level1 then
            [ "Activate the circles by placing tiles on the grid." |> Layout.text []
            , "Energy will flow along the tiles." |> Layout.text []
            , "If the direction is ambiguous, it will always go straight." |> Layout.text []
            ]
                |> View.card [ Layout.gap 16 ]

        else
            [ "Edit Levels" |> View.cardTitle
            , model.levels
                |> Dict.get (model.level |> Level.previous |> Level.toString)
                |> Maybe.withDefault Dict.empty
                |> View.savedLevels { level = model.level |> Level.previous }
                    (\stage ->
                        LoadStage
                            { level = model.level |> Level.previous
                            , stage = stage
                            }
                    )
            ]
                |> View.card [ Layout.gap 16 ]
      ]
        |> Layout.column
            [ Layout.gap 16
            , Html.Attributes.style "padding" "1rem"
            , Html.Attributes.style "width" ((Config.cellSize * 6 |> String.fromInt) ++ "px")
            ]
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

       else
        model.selected
            |> Maybe.andThen
                (\selected ->
                    model.levels
                        |> Dict.get (model.level |> Level.previous |> Level.toString)
                        |> Maybe.withDefault Dict.empty
                        |> TileSelect.toHtml
                            { removeTile = RemoveTile
                            , selected = selected
                            , unselect = Unselect
                            , game = model.game
                            , level = model.level
                            , placeModule = PlaceModule
                            , levels = model.levels
                            }
                        |> Just
                )
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
                               , Html.Attributes.style "backdrop-filter" "blur(2px)"
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
            [ Layout.asEl
            , Html.Attributes.style "position" "relative"
            , Html.Attributes.style "color" Color.fontColor
            , Html.Attributes.style "background" (Color.background model.level)
            , Html.Attributes.style "font-family" "sans-serif"
            , Html.Attributes.style "height" "100%"
            , Layout.contentCentered
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ( x, y ) ->
            ( (if x >= 0 && x <= 3 && y >= 0 && y <= 3 then
                model.game
                    |> Maybe.map
                        (\game ->
                            case model.level of
                                Level1 ->
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

                                _ ->
                                    case game.stage.grid |> Dict.get ( x, y ) of
                                        Just (ConnectionCell _) ->
                                            { model | selected = Just ( x, y ) }

                                        Nothing ->
                                            { model | selected = Just ( x, y ) }

                                        _ ->
                                            model
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
                                    |> Game.update model.level
                                        (model.levels
                                            |> Dict.get (model.level |> Level.previous |> Level.toString)
                                            |> Maybe.withDefault Dict.empty
                                        )
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
                                m.level
                                    |> Level.next
                                    |> Maybe.andThen
                                        (\level ->
                                            m
                                                |> loadStage { level = level, stage = 1 }
                                        )
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

        RemoveTile pos ->
            ( model.game
                |> Maybe.map
                    (\game ->
                        game.stage.grid
                            |> Dict.remove pos
                            |> (\grid ->
                                    { model
                                        | game =
                                            { game
                                                | stage = game.stage |> (\stage -> { stage | grid = grid })
                                            }
                                                |> Just
                                        , selected = Nothing
                                    }
                               )
                    )
                |> Maybe.withDefault model
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
