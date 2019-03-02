module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Option)
import Element.Background as Background
import Element.Font as Font
import LittleWorldPuzzler.State as State exposing (Action)
import LittleWorldPuzzler.State.Finished as FinishedState
import LittleWorldPuzzler.State.Playing as PlayingState
import LittleWorldPuzzler.State.Prepairing as PreparingState
import LittleWorldPuzzler.State.Ready as ReadyState
import LittleWorldPuzzler.State.Replaying as ReplayingState
import Random
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
    { scale : Float
    , portraitMode : Bool
    }


type Model
    = Preparing PreparingState.Model
    | Ready ( ReadyState.Model, Config )
    | Playing ( PlayingState.Model, Config )
    | Replaying ( ReplayingState.Model, Config )
    | Finished ( FinishedState.Model, Config )


type Msg
    = PlayingSpecific PlayingState.Msg
    | ReadySpecific ReadyState.Msg
    | PreparingSpecific PreparingState.Msg
    | ReplayingSpecific ReplayingState.Msg
    | FinishedSpecific FinishedState.Msg
    | Resized Config
    | Restart


calcPortraitMode : { height : Float, width : Float } -> Bool
calcPortraitMode dim =
    dim.height > dim.width


calcScale : { height : Float, width : Float } -> Float
calcScale dim =
    if dim.width / dim.height > width / height then
        dim.height / height

    else
        dim.width / width



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    ( Preparing { scale = Nothing, seed = Nothing, portraitMode = False }
    , Cmd.batch
        [ Random.generate (PreparingSpecific << PreparingState.GotSeed)
            Random.independentSeed
        , Task.perform
            (\{ viewport } ->
                { width = viewport.width, height = viewport.height }
                    |> (\dim ->
                            Resized
                                { scale = calcScale dim
                                , portraitMode = calcPortraitMode dim
                                }
                       )
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
        PreparingSpecific preparingMsg ->
            case model of
                Preparing preparingModel ->
                    PreparingState.update preparingMsg preparingModel
                        |> State.apply
                            { exit = init ()
                            , modelMapper = Preparing
                            , msgMapper = PreparingSpecific
                            , transition =
                                \{ scale, portraitMode, seed } ->
                                    ReadyState.init seed
                                        |> (\( m, c ) ->
                                                ( Ready
                                                    ( m
                                                    , { scale = scale
                                                      , portraitMode = portraitMode
                                                      }
                                                    )
                                                , c |> Cmd.map ReadySpecific
                                                )
                                           )
                            }

                _ ->
                    defaultCase

        ReadySpecific readyMsg ->
            case model of
                Ready ( readyModel, config ) ->
                    ReadyState.update readyMsg readyModel
                        |> State.apply
                            { exit = init ()
                            , modelMapper = \m -> Ready ( m, config )
                            , msgMapper = ReadySpecific
                            , transition =
                                PlayingState.init
                                    >> (\( m, c ) ->
                                            ( Playing ( m, config ), c |> Cmd.map PlayingSpecific )
                                       )
                            }

                _ ->
                    defaultCase

        PlayingSpecific playingMsg ->
            case model of
                Playing ( playingModel, config ) ->
                    PlayingState.update playingMsg playingModel
                        |> State.apply
                            { exit = init ()
                            , modelMapper = \m -> Playing ( m, config )
                            , msgMapper = PlayingSpecific
                            , transition =
                                FinishedState.init
                                    >> (\( m, c ) ->
                                            ( Finished ( m, config ), c |> Cmd.map FinishedSpecific )
                                       )
                            }

                _ ->
                    defaultCase

        FinishedSpecific finishedMsg ->
            case model of
                Finished ( finishedModel, config ) ->
                    FinishedState.update finishedMsg finishedModel
                        |> State.apply
                            { exit = init ()
                            , modelMapper = \m -> Finished ( m, config )
                            , msgMapper = FinishedSpecific
                            , transition =
                                \m ->
                                    ( Replaying ( m, config )
                                    , Cmd.none
                                    )
                            }

                _ ->
                    defaultCase

        ReplayingSpecific replayingMsg ->
            case model of
                Replaying ( replayingModel, config ) ->
                    ReplayingState.update replayingMsg replayingModel
                        |> State.apply
                            { exit = init ()
                            , modelMapper = \m -> Replaying ( m, config )
                            , msgMapper = ReplayingSpecific
                            , transition = never
                            }

                _ ->
                    defaultCase

        Restart ->
            init ()

        Resized { scale, portraitMode } ->
            ( case model of
                Playing ( playingModel, config ) ->
                    Playing ( playingModel, { config | scale = scale, portraitMode = portraitMode } )

                Replaying ( replayingModel, config ) ->
                    Replaying ( replayingModel, { config | scale = scale, portraitMode = portraitMode } )

                Finished ( finishedModel, config ) ->
                    Finished ( finishedModel, { config | scale = scale, portraitMode = portraitMode } )

                Ready ( readyModel, config ) ->
                    Ready ( readyModel, { config | scale = scale, portraitMode = portraitMode } )

                Preparing ({ seed } as prepairingModel) ->
                    case seed of
                        Just s ->
                            Ready
                                ( ReadyState.init s
                                    |> Tuple.first
                                , { scale = scale
                                  , portraitMode = portraitMode
                                  }
                                )

                        Nothing ->
                            Preparing
                                { prepairingModel
                                    | scale = Just scale
                                    , portraitMode = portraitMode
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
                |> (\dim ->
                        Resized
                            { scale = calcScale dim
                            , portraitMode = calcPortraitMode dim
                            }
                   )
        )



----------------------
-- View
----------------------


view : Model -> Browser.Document Msg
view model =
    let
        forceHover : Bool -> List Option
        forceHover bool =
            if bool then
                [ Element.forceHover
                ]

            else
                []

        ( content, options ) =
            case model of
                Playing ( playingModel, { scale, portraitMode } ) ->
                    ( PlayingState.view scale Restart PlayingSpecific playingModel
                    , forceHover portraitMode
                    )

                Replaying ( replayingModel, { scale, portraitMode } ) ->
                    ( ReplayingState.view scale Restart ReplayingSpecific replayingModel
                    , forceHover portraitMode
                    )

                Finished ( finishedModel, { scale, portraitMode } ) ->
                    ( FinishedState.view scale Restart FinishedSpecific finishedModel
                    , forceHover portraitMode
                    )

                Ready ( readyModel, { scale, portraitMode } ) ->
                    ( ReadyState.view scale Restart ReadySpecific readyModel
                    , forceHover portraitMode
                    )

                Preparing _ ->
                    ( Element.text ""
                    , []
                    )
    in
    { title = "Little World Puzzler"
    , body =
        List.singleton <|
            Element.layoutWith
                { options = options }
                [ Font.family
                    [ Font.external
                        { url = "font.css"
                        , name = "Noto Emoji"
                        }
                    ]
                , Background.color <| Element.rgb255 44 48 51
                ]
            <|
                content
    }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
