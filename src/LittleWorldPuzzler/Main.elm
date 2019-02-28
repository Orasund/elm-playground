module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Modifier as Modifier exposing (Modifier(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.State.Playing as PlayingState
import LittleWorldPuzzler.State.Prepairing as PrepairingState
import LittleWorldPuzzler.State.Replaying as ReplayingState
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)


height : Float
height =
    873


width : Float
width =
    608



----------------------
-- Model
----------------------


type alias Config =
    { scale : Float }


type Model
    = Prepairing PrepairingState.Model
    | Playing ( PlayingState.Model, Config )
    | Replaying ( ReplayingState.Model, Config )


type Msg
    = PlayingSpecific PlayingState.Msg
    | PrepairingSpecific PrepairingState.Msg
    | ReplayingSpecific ReplayingState.Msg
    | Resized Float
    | Restart


calcScale : { height : Float, width : Float } -> Float
calcScale dim =
    if dim.width > dim.height then
        dim.height / height

    else
        dim.width / width



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    ( Prepairing { scale = Nothing, seed = Nothing }
    , Cmd.batch
        [ Random.generate (PrepairingSpecific << PrepairingState.GotSeed)
            Random.independentSeed
        , Task.perform
            (\{ viewport } ->
                { width = viewport.width, height = viewport.height }
                    |> calcScale
                    |> Resized
            )
            Dom.getViewport
        ]
    )



----------------------
-- Update
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( model, Cmd.none )
    in
    case msg of
        PrepairingSpecific prepairingMsg ->
            case model of
                Prepairing prepairingModel ->
                    PrepairingState.update
                        (\scale seed ->
                            Playing
                                ( PlayingState.init seed
                                , { scale = scale }
                                )
                        )
                        Prepairing
                        prepairingMsg
                        prepairingModel
                        |> (\( a, b ) ->
                                ( a, b |> Cmd.map PrepairingSpecific )
                           )

                _ ->
                    defaultCase

        ReplayingSpecific replayingMsg ->
            case model of
                Replaying ( replayingModel, config ) ->
                    ReplayingState.update replayingMsg replayingModel
                        |> (\( a, b ) ->
                                ( Replaying ( a, config ), b |> Cmd.map ReplayingSpecific )
                           )

                _ ->
                    defaultCase

        PlayingSpecific playingMsg ->
            case model of
                Playing ( playingModel, config ) ->
                    PlayingState.update
                        (\m -> Replaying ( m, config ))
                        (\m -> Playing ( m, config ))
                        playingMsg
                        playingModel
                        |> (\( a, b ) ->
                                ( a, b |> Cmd.map PlayingSpecific )
                           )

                _ ->
                    defaultCase

        Restart ->
            init ()

        Resized scale ->
            ( case model of
                Playing ( playingModel, config ) ->
                    Playing ( playingModel, { config | scale = scale } )

                Replaying ( replayingModel, config ) ->
                    Replaying ( replayingModel, { config | scale = scale } )

                Prepairing ({ seed } as prepairingModel) ->
                    case seed of
                        Just s ->
                            Playing
                                ( PlayingState.init s
                                , { scale = scale
                                  }
                                )

                        Nothing ->
                            Prepairing
                                { prepairingModel
                                    | scale = Just scale
                                }
            , Cmd.none
            )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize
        (\w h ->
            { width = toFloat w, height = toFloat h }
                |> calcScale
                |> Resized
        )



----------------------
-- View
----------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Little World Puzzler"
    , body =
        List.singleton <|
            Element.layout
                [ Font.family
                    [ Font.external
                        { url = "font.css"
                        , name = "Noto Emoji"
                        }
                    ]
                , Background.color <| Element.rgb255 44 48 51
                ]
            <|
                case model of
                    Playing ( playingModel, { scale } ) ->
                        PlayingState.view scale Restart PlayingSpecific playingModel

                    Replaying ( replayingModel, { scale } ) ->
                        ReplayingState.view scale Restart ReplayingSpecific replayingModel

                    Prepairing _ ->
                        Element.text ""
    }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
