module GreenFields.Page.InGame exposing (Model, Msg, init, subscriptions, update, view)

import Bag
import Browser
import Color as C
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Firestore exposing (Error)
import GreenFields.Data.Building as Building exposing (Building)
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.Data.Resource as Resource
import GreenFields.Data.Tile as Tile exposing (Tile)
import GreenFields.View.Card as Card
import GreenFields.View.Color as Color
import GreenFields.View.Tile as Tile
import Html exposing (Html)
import Task exposing (Task)
import Time exposing (Posix)


type alias Model =
    { game : Game
    , center : ( Int, Int )
    , selected : Maybe ( Int, Int )
    , timestamp : Posix
    , minutesLeft : Int
    }


type Msg
    = Restart
    | Select ( Int, Int )
    | Restore ( Int, Int )
    | Build Building ( Int, Int )
    | Demolish ( Int, Int )
    | GotTime Posix
    | UpdatedDB (Result Error Tile)
    | MinutePassed
    | Idle


init :
    { center : ( Int, Int )
    , timestamp : Posix
    , board : Dict ( Int, Int ) Tile
    }
    -> Model
init args =
    restart
        { game = Game.fromBoard args.board
        , center = args.center
        , timestamp = args.timestamp
        }


restart : { game : Game, center : ( Int, Int ), timestamp : Posix } -> Model
restart args =
    { game =
        args.game
            |> Game.getBoard
            |> Game.fromBoard
    , center = args.center
    , selected = Nothing
    , timestamp = args.timestamp
    , minutesLeft = 5
    }


restore :
    { modelMapper : Model -> model
    , restart : ( model, Cmd msg )
    , msgMapper : Msg -> msg
    }
    -> ( Int, Int )
    -> Model
    -> ( model, Cmd msg )
restore args pos model =
    let
        ( task, game ) =
            model.game
                |> Game.restore
                    { position = pos
                    , timestamp = model.timestamp
                    }
    in
    ( args.modelMapper { model | game = game }
    , task
        |> Maybe.map (Task.attempt (UpdatedDB >> args.msgMapper))
        |> Maybe.withDefault Cmd.none
    )


select :
    { modelMapper : Model -> model
    , restart : ( model, Cmd msg )
    , msgMapper : Msg -> msg
    }
    -> ( Int, Int )
    -> Model
    -> ( model, Cmd msg )
select args pos model =
    ( args.modelMapper { model | selected = Just pos, center = pos }
    , Cmd.none
    )


update :
    { modelMapper : Model -> model
    , restart : ( model, Cmd msg )
    , msgMapper : Msg -> msg
    }
    -> Msg
    -> Model
    -> ( model, Cmd msg )
update args msg model =
    case msg of
        GotTime timestamp ->
            ( args.modelMapper { model | timestamp = timestamp }, Cmd.none )

        Select pos ->
            if model.selected == Just pos then
                restore args pos model

            else
                select args pos model

        Restore pos ->
            restore args pos model

        Build building pos ->
            let
                ( task, game ) =
                    model.game
                        |> Game.build
                            { building = building
                            , position = pos
                            , timestamp = model.timestamp
                            }
            in
            ( args.modelMapper { model | game = game }
            , task
                |> Maybe.map (Task.attempt (UpdatedDB >> args.msgMapper))
                |> Maybe.withDefault Cmd.none
            )

        Demolish pos ->
            let
                ( task, game ) =
                    model.game
                        |> Game.demolish pos
            in
            ( args.modelMapper
                { model
                    | game = game
                }
            , task
                |> Task.attempt (always Idle >> args.msgMapper)
            )

        MinutePassed ->
            if model.minutesLeft == 1 then
                args.restart

            else
                ( args.modelMapper { model | minutesLeft = model.minutesLeft - 1 }
                , Cmd.none
                )

        UpdatedDB result ->
            case result of
                Ok tile ->
                    ( args.modelMapper { model | game = model.game |> Game.update tile }
                    , Cmd.none
                    )

                Err _ ->
                    ( args.modelMapper model, Cmd.none )

        Restart ->
            args.restart

        Idle ->
            ( args.modelMapper model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        radius =
            Game.viewRadius

        ( centerX, centerY ) =
            model.center
    in
    [ model.game
        |> Game.getResources
        |> List.map
            (\( resource, int ) ->
                (int |> String.fromInt)
                    ++ " "
                    ++ (resource |> Resource.toString)
            )
        |> String.join ", "
        |> Element.text
        |> Element.el
            [ Element.centerX
            , Color.white
                |> C.toRgba
                |> Element.fromRgb
                |> Font.color
            ]
    , [ List.range (centerY - radius) (centerY + radius)
            |> List.map
                (\y ->
                    List.range (centerX - radius) (centerX + radius)
                        |> List.map
                            (\x ->
                                Tile.view
                                    { game = model.game
                                    , pos = ( x, y )
                                    , onClick = Select ( x, y )
                                    , timestamp = model.timestamp
                                    }
                            )
                        |> Element.row [ Element.spacing 2 ]
                )
            |> Element.column
                [ Element.spacing 2
                , Element.alignRight
                , Element.centerY
                ]
            |> Element.el
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                ]
      , Card.view
            { selected = model.selected
            , game = model.game
            , onRestore =
                model.selected
                    |> Maybe.map Restore
                    |> Maybe.withDefault Restart
            , onBuild =
                model.selected
                    |> Maybe.map (\pos building -> Build building pos)
                    |> Maybe.withDefault (always Restart)
            , onDemolish =
                model.selected
                    |> Maybe.map Demolish
                    |> Maybe.withDefault Restart
            , timestamp = model.timestamp
            }
            |> Element.el
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                ]
      ]
        |> Element.row
            [ Element.width <| Element.fill
            , Element.spacing 2
            ]
    , [ " Restarts in "
            ++ String.fromInt model.minutesLeft
            ++ " Minutes "
            |> Element.text
            |> Element.el [ Element.alignRight ]
            |> Element.el [ Element.width Element.fill ]
      , Input.button
            [ Color.black
                |> C.toRgba
                |> Element.fromRgb
                |> Font.color
            , Font.bold
            ]
            { onPress = Just Restart
            , label = "[Restart Now]" |> Element.text
            }
            |> Element.el [ Element.width Element.fill ]
      ]
        |> Element.row
            [ Element.centerX
            , Color.white
                |> C.toRgba
                |> Element.fromRgb
                |> Font.color
            , Element.width Element.fill
            ]
    ]
        |> Element.column
            [ Element.centerY
            , Element.width Element.fill
            , Element.spacing 14
            ]
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Color.green
                |> C.toRgba
                |> Element.fromRgb
                |> Background.color
            , Font.size 14
            , Font.family [ Font.monospace ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 * 60)
        (always MinutePassed)
