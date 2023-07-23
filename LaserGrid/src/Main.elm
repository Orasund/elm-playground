module Main exposing (..)

import Browser
import Cell exposing (Cell(..), Cell1)
import Config
import Dict exposing (Dict)
import Game exposing (Game(..), Module)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Time
import View
import View.Svg


type alias Model =
    { game : Game
    , modules : Dict Int Module
    , updating : Bool
    , levels : Dict Int (Dict ( Int, Int ) Cell1)
    , level : Int
    , selected : Maybe ( Int, Int )
    }


type Msg
    = Toggle ( Int, Int )
    | PlaceModule { moduleId : Int, rotation : Int }
    | Unselect
    | UpdateGrid
    | NextLevel


init : () -> ( Model, Cmd Msg )
init () =
    let
        level =
            1
    in
    ( { game = Game.Generate.fromId level
      , modules = Dict.empty
      , updating = False
      , levels = Dict.empty
      , level = level
      , selected = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    [ [ model.game |> View.grid { levels = model.levels, onToggle = Toggle }
      , model.levels
            |> Dict.toList
            |> List.map
                (\( _, grid ) ->
                    grid
                        |> View.Svg.grid
                            { width = Config.cellSize
                            , height = Config.cellSize
                            }
                )
            |> Layout.row [ Layout.gap 8 ]
      ]
        |> Layout.column [ Layout.gap 16 ]
    , (if (model.updating == False) && Game.isSolved model.game then
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
        , Game.modules
            |> Dict.keys
            |> List.map
                (\id ->
                    List.range 0 3
                        |> List.map
                            (\rotate ->
                                { sort =
                                    { moduleId = id
                                    , rotation = rotate
                                    }
                                , active = []
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
                    |> View.dialog
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
                    Level1 stage ->
                        stage.grid
                            |> Dict.update ( x, y )
                                (\maybe ->
                                    case maybe of
                                        Just (ConnectionCell _) ->
                                            Nothing

                                        Nothing ->
                                            Just (ConnectionCell { active = [], sort = () })

                                        _ ->
                                            maybe
                                )
                            |> (\grid -> { stage | grid = grid })
                            |> Level1
                            |> (\grid -> { model | game = grid })

                    Level2 stage ->
                        case stage.grid |> Dict.get ( x, y ) of
                            Just (ConnectionCell _) ->
                                { stage
                                    | grid =
                                        stage.grid
                                            |> Dict.remove ( x, y )
                                }
                                    |> Level2
                                    |> (\grid -> { model | game = grid })

                            Nothing ->
                                { model | selected = Just ( x, y ) }

                            _ ->
                                model

               else
                model
              )
                |> (\m -> { m | updating = True })
            , Cmd.none
            )

        PlaceModule { moduleId, rotation } ->
            ( case model.game of
                Level1 _ ->
                    model

                Level2 stage ->
                    { stage
                        | grid =
                            stage.grid
                                |> Dict.insert (model.selected |> Maybe.withDefault ( 0, 0 ))
                                    ({ sort =
                                        { moduleId = moduleId
                                        , rotation = rotation
                                        }
                                     , active = []
                                     }
                                        |> ConnectionCell
                                    )
                    }
                        |> Level2
                        |> (\grid ->
                                { model
                                    | game = grid
                                    , updating = True
                                    , selected = Nothing
                                }
                           )
            , Cmd.none
            )

        UpdateGrid ->
            let
                ( newGrid, updating ) =
                    model.game
                        |> Game.update
            in
            ( { model
                | game = newGrid
                , updating = updating
              }
            , Cmd.none
            )

        NextLevel ->
            let
                level =
                    model.level + 1

                grid =
                    Game.Generate.fromId level
            in
            ( { model
                | game = grid
                , levels =
                    model.levels
                        |> Dict.insert model.level
                            (case model.game of
                                Level1 stage ->
                                    stage.grid

                                Level2 _ ->
                                    Debug.todo "implement recursion"
                            )
                , level = level
              }
            , Cmd.none
            )

        Unselect ->
            ( { model | selected = Nothing }, Cmd.none )


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
