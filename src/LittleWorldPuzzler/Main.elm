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
import LittleWorldPuzzler.State.Playing as PlayingState
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import Process
import Random exposing (Generator, Seed)
import Task


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


type alias PrepairingModel =
    { scale : Maybe Float, seed : Maybe Seed }


type Model
    = Prepairing PrepairingModel
    | Playing ( PlayingState.Model, Config )


type PrepairingMsg
    = GotSeed Seed


type Msg
    = PlayingSpecific PlayingState.Msg
    | PrepairingSpecific PrepairingMsg
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
        [ Random.generate (PrepairingSpecific << GotSeed) Random.independentSeed
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


prepairingUpdate : PrepairingMsg -> PrepairingModel -> ( Model, Cmd Msg )
prepairingUpdate msg model =
    case msg of
        GotSeed seed ->
            case model.scale of
                Just scale ->
                    ( Playing
                        ( PlayingState.init seed
                        , { scale = scale }
                        )
                    , Cmd.none
                    )

                Nothing ->
                    ( Prepairing { model | seed = Just seed }
                    , Cmd.none
                    )


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
                    prepairingUpdate prepairingMsg prepairingModel

                _ ->
                    defaultCase

        PlayingSpecific playingMsg ->
            case model of
                Playing ( playingModel, config ) ->
                    PlayingState.update playingMsg playingModel
                        |> (\( a, b ) ->
                                ( Playing ( a, config ), b |> Cmd.map PlayingSpecific )
                           )

                _ ->
                    defaultCase

        Restart ->
            init ()

        Resized scale ->
            ( case model of
                Playing ( playingModel, config ) ->
                    Playing ( playingModel, { config | scale = scale } )

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
