module Main exposing (main)

import Action
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Option)
import Element.Background as Background
import Element.Font as Font
import LittleWorldPuzzler.State.Finished as FinishedState
import LittleWorldPuzzler.State.Playing as PlayingState
import LittleWorldPuzzler.State.Prepairing as PreparingState
import LittleWorldPuzzler.State.Ready as ReadyState
import LittleWorldPuzzler.State.Replaying as ReplayingState
import Random
import Task


height : Float
height =
    925


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
    case ( msg, model ) of
        ( PreparingSpecific preparingMsg, Preparing preparingModel ) ->
            PreparingState.update preparingMsg preparingModel
                |> Action.config
                |> Action.withUpdate Preparing never
                |> Action.withTransition
                    (\{ scale, portraitMode, seed } ->
                        ReadyState.init seed
                            |> (\( m, c ) ->
                                    ( ( m
                                      , { scale = scale
                                        , portraitMode = portraitMode
                                        }
                                      )
                                    , c
                                    )
                               )
                    )
                    Ready
                    ReadySpecific
                |> Action.apply

        ( ReadySpecific readyMsg, Ready ( readyModel, config ) ) ->
            ReadyState.update readyMsg readyModel
                |> Action.config
                |> Action.withUpdate (\m -> Ready ( m, config )) ReadySpecific
                |> Action.withTransition
                    (PlayingState.init
                        >> (\( m, c ) ->
                                ( ( m, config )
                                , c
                                )
                           )
                    )
                    Playing
                    PlayingSpecific
                |> Action.apply

        ( PlayingSpecific playingMsg, Playing ( playingModel, config ) ) ->
            PlayingState.update playingMsg playingModel
                |> Action.config
                |> Action.withUpdate (\m -> Playing ( m, config )) PlayingSpecific
                |> Action.withTransition
                    (FinishedState.init
                        >> (\( m, c ) ->
                                ( ( m, config )
                                , c
                                )
                           )
                    )
                    Finished
                    FinishedSpecific
                |> Action.withExit (init ())
                |> Action.apply

        ( FinishedSpecific finishedMsg, Finished ( finishedModel, config ) ) ->
            FinishedState.update finishedMsg finishedModel
                |> Action.config
                |> Action.withUpdate
                    (\m -> Finished ( m, config ))
                    FinishedSpecific
                |> Action.withTransition
                    (\m ->
                        ( ( m, config )
                        , Cmd.none
                        )
                    )
                    Replaying
                    never
                |> Action.apply

        ( ReplayingSpecific replayingMsg, Replaying ( replayingModel, config ) ) ->
            ReplayingState.update replayingMsg replayingModel
                |> Action.config
                |> Action.withUpdate (\m -> Replaying ( m, config )) never
                |> Action.apply

        ( Restart, _ ) ->
            init ()

        ( Resized { scale, portraitMode }, _ ) ->
            ( case model of
                Playing ( playingModel, config ) ->
                    Playing
                        ( playingModel
                        , { config | scale = scale, portraitMode = portraitMode }
                        )

                Replaying ( replayingModel, config ) ->
                    Replaying
                        ( replayingModel
                        , { config | scale = scale, portraitMode = portraitMode }
                        )

                Finished ( finishedModel, config ) ->
                    Finished
                        ( finishedModel
                        , { config | scale = scale, portraitMode = portraitMode }
                        )

                Ready ( readyModel, config ) ->
                    Ready
                        ( readyModel
                        , { config | scale = scale, portraitMode = portraitMode }
                        )

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

        _ ->
            ( model, Cmd.none )



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


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
