module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Dict exposing (Dict)
import Grid exposing (Grid(..))
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Module)
import Time


type alias Model =
    { grid : Grid
    , modules : Dict Int Module
    , updating : Bool
    , targets : List ( Int, Int )
    , level : Int
    }


type Msg
    = Toggle ( Int, Int )
    | UpdateGrid
    | NextLevel


init : () -> ( Model, Cmd Msg )
init () =
    let
        level =
            1

        grid =
            Level.fromInt level

        targets =
            grid
                |> Dict.filter (\_ -> (==) (Target False))
                |> Dict.keys
    in
    ( { grid = grid |> Stage1
      , modules = Dict.empty
      , targets = targets
      , updating = False
      , level = level
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    [ List.range -1 4
        |> List.map
            (\y ->
                List.range -1 4
                    |> List.map
                        (\x ->
                            model.grid
                                |> Grid.getEmoji ( x, y )
                                |> Maybe.withDefault ""
                                |> Layout.text
                                    (Layout.asButton
                                        { onPress = Just (Toggle ( x, y ))
                                        , label = "Toggle " ++ String.fromInt x ++ "," ++ String.fromInt y
                                        }
                                        ++ [ Html.Attributes.style "width" "64px"
                                           , Html.Attributes.style "height" "64px"
                                           , Html.Attributes.style "font-size" "56px"
                                           ]
                                        ++ Layout.centered
                                    )
                        )
                    |> Layout.row []
            )
        |> Layout.column []
    , if
        model.updating
            == False
            && List.all
                (\pos ->
                    case model.grid |> Grid.toDict |> Dict.get pos of
                        Just (Target True) ->
                            True

                        _ ->
                            False
                )
                model.targets
      then
        [ "You Win"
            |> Layout.text []
        , Layout.textButton []
            { label = "Next Level"
            , onPress = Just NextLevel
            }
        ]
            |> Layout.column
                (Layout.centered
                    ++ [ Html.Attributes.style "width" "200px"
                       , Html.Attributes.style "height" "75px"
                       , Html.Attributes.style "background" "white"
                       , Html.Attributes.style "border" "1px solid black"
                       , Html.Attributes.style "position" "absolute"
                       , Layout.gap 8
                       ]
                )
            |> Layout.el
                (Layout.centered
                    ++ [ Html.Attributes.style "width" "100%"
                       , Html.Attributes.style "height" "100%"
                       , Html.Attributes.style "position" "absolute"
                       ]
                )

      else
        Layout.none
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
            ( { model
                | grid =
                    if x >= 0 && x <= 3 && y >= 0 && y <= 3 then
                        case model.grid of
                            Stage1 dict ->
                                dict
                                    |> Dict.update ( x, y )
                                        (\maybe ->
                                            case maybe of
                                                Just (Glass _) ->
                                                    Nothing

                                                Nothing ->
                                                    Just (Glass [])

                                                _ ->
                                                    maybe
                                        )
                                    |> Stage1

                            _ ->
                                model.grid

                    else
                        model.grid
                , updating = True
              }
            , Cmd.none
            )

        UpdateGrid ->
            let
                ( newGrid, updating ) =
                    model.grid
                        |> Grid.update
            in
            ( { model
                | grid = newGrid
                , updating = updating
              }
            , Cmd.none
            )

        NextLevel ->
            let
                level =
                    model.level + 1

                grid =
                    Level.fromInt level

                targets =
                    grid
                        |> Dict.filter (\_ -> (==) (Target False))
                        |> Dict.keys
            in
            ( { model
                | grid = Stage1 grid
                , targets = targets
                , level = level
              }
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
