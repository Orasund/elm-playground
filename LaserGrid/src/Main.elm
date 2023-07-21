module Main exposing (..)

import Browser
import Cell exposing (Cell(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level
import Set exposing (Set)
import Time


type alias Model =
    { grid : Dict ( Int, Int ) Cell
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
    ( { grid = grid
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
                                |> Dict.get ( x, y )
                                |> Maybe.map Cell.toEmoji
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
                    case model.grid |> Dict.get pos of
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


tick : ( ( Int, Int ), Cell ) -> Dict ( Int, Int ) Cell -> Cell
tick ( ( x, y ), cell ) dict =
    let
        hasEnergy pos =
            case Dict.get pos dict of
                Just (Glass (_ :: _)) ->
                    True

                Just Laser ->
                    True

                Just (Target True) ->
                    True

                _ ->
                    False

        posFromDir ( x0, y0 ) =
            ( x + x0, y + y0 )

        neighboringDir =
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]

        neighborsDir =
            neighboringDir
                |> List.filterMap
                    (\dir ->
                        case Dict.get (posFromDir dir) dict of
                            Just (Glass _) ->
                                Just dir

                            Just Laser ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )

        sendsEnergyFromDir : ( Int, Int ) -> Bool
        sendsEnergyFromDir dir =
            case dict |> Dict.get (posFromDir dir) of
                Just (Glass to) ->
                    to |> List.member ( x, y )

                Just Laser ->
                    True

                _ ->
                    False
    in
    case cell of
        Glass _ ->
            case neighborsDir of
                [ dir1, dir2 ] ->
                    if sendsEnergyFromDir dir1 then
                        Glass [ posFromDir dir2 ]

                    else if sendsEnergyFromDir dir2 then
                        Glass [ posFromDir dir1 ]

                    else
                        Glass []

                _ ->
                    neighboringDir
                        |> List.filter sendsEnergyFromDir
                        |> (\list ->
                                list
                                    |> List.map
                                        (\( x0, y0 ) ->
                                            ( x - x0, y - y0 )
                                        )
                                    |> Glass
                           )

        Target _ ->
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
                |> List.any sendsEnergyFromDir
                |> Target

        _ ->
            cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ( x, y ) ->
            ( { model
                | grid =
                    if x >= 0 && x <= 3 && y >= 0 && y <= 3 then
                        model.grid
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

                    else
                        model.grid
                , updating = True
              }
            , Cmd.none
            )

        UpdateGrid ->
            let
                newGrid =
                    model.grid
                        |> Dict.map
                            (\pos cell ->
                                tick ( pos, cell ) model.grid
                            )
            in
            ( { model
                | grid = newGrid
                , updating = Dict.toList newGrid /= Dict.toList model.grid
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
                | grid = grid
                , targets = targets
                , level = level
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.updating then
        Time.every 100 (\_ -> UpdateGrid)

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
