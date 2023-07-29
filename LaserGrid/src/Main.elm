module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Dict exposing (Dict)
import Game exposing (Game(..), SavedLevel)
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
            Game.Generate.fromId level
    in
    { model
        | game = grid
        , level = level
    }


view : Model -> Html Msg
view model =
    [ [ model.game
            |> Maybe.map
                (View.grid []
                    { levels = model.levels
                    , onToggle = Toggle
                    }
                )
            |> Maybe.withDefault View.gameWon
      , [ Layout.text [] "Edit Stages"
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
        [ "You Win"
            |> Layout.text []
        , Layout.textButton []
            { label = "Next Level"
            , onPress = Just NextLevel
            }
        ]
            |> Just

       else if model.selected /= Nothing then
        [ "Select a tile you want to place"
            |> Layout.text []
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
                                    |> View.tile2 model.levels
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
        , Layout.textButton []
            { label = "Cancel"
            , onPress = Just Unselect
            }
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
    ]
        |> Html.div (Layout.centered ++ [ Layout.asEl, Html.Attributes.style "position" "relative" ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ( x, y ) ->
            ( (if x >= 0 && x <= 3 && y >= 0 && y <= 3 then
                case model.game of
                    Just (Level1 stage) ->
                        stage.grid
                            |> Dict.update ( x, y )
                                (\maybe ->
                                    case maybe of
                                        Just (ConnectionCell _) ->
                                            Nothing

                                        Nothing ->
                                            Just
                                                (ConnectionCell
                                                    { sendsTo = Dict.empty
                                                    }
                                                )

                                        _ ->
                                            maybe
                                )
                            |> (\grid -> { stage | grid = grid })
                            |> Level1
                            |> (\grid -> { model | game = Just grid })

                    Just (Level2 stage) ->
                        case stage.grid |> Dict.get ( x, y ) |> Debug.log "cell" of
                            Just (ConnectionCell _) ->
                                { stage
                                    | grid =
                                        stage.grid
                                            |> Dict.remove ( x, y )
                                }
                                    |> Level2
                                    |> (\grid -> { model | game = Just grid })

                            Nothing ->
                                { model | selected = Just ( x, y ) }

                            _ ->
                                model

                    Nothing ->
                        model

               else
                model
              )
                |> (\m -> { m | updating = True })
            , Cmd.none
            )

        PlaceModule { moduleId, rotation } ->
            ( case model.game of
                Just (Level1 _) ->
                    model

                Just (Level2 stage) ->
                    { stage
                        | grid =
                            stage.grid
                                |> Dict.insert (model.selected |> Maybe.withDefault ( 0, 0 ))
                                    ({ moduleId = moduleId
                                     , rotation = rotation
                                     , sendsTo = Dict.empty
                                     }
                                        |> ConnectionCell
                                    )
                    }
                        |> Level2
                        |> (\game ->
                                { model
                                    | game = Just game
                                    , updating = True
                                    , selected = Nothing
                                }
                           )

                Nothing ->
                    model
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
            ( model |> loadLevel level
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
