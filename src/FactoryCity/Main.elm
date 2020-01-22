module FactoryCity.Main exposing (main)

import Action
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Option)
import Element.Background as Background
import Element.Font as Font
import FactoryCity.State.Playing as PlayingState
import FactoryCity.State.Prepairing as PreparingState
import FactoryCity.State.Ready as ReadyState
import FactoryCity.View.Shade as Shade
import Framework
import Framework.Grid as Grid
import Html
import Html.Attributes as Attributes
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


type Msg
    = PlayingSpecific PlayingState.Msg
    | ReadySpecific ReadyState.Msg
    | PreparingSpecific PreparingState.Msg
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
                |> Action.withExit (init ())
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
subscriptions model =
    Sub.batch
        ([ onResize
            (\w h ->
                { width = toFloat w, height = toFloat h }
                    |> (\dim ->
                            Resized
                                { scale = calcScale dim
                                , portraitMode = calcPortraitMode dim
                                }
                       )
            )
         ]
            ++ (case model of
                    Playing playingModel ->
                        List.singleton <|
                            Sub.map PlayingSpecific <|
                                PlayingState.subscriptions <|
                                    Tuple.first <|
                                        playingModel

                    _ ->
                        []
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

        ( maybeShade, content ) =
            case model of
                Playing ( playingModel, { scale } ) ->
                    PlayingState.view scale Restart PlayingSpecific playingModel

                Ready ( readyModel, { scale } ) ->
                    ReadyState.view scale Restart ReadySpecific readyModel

                Preparing _ ->
                    ( Nothing, [ Element.text "" ] )

        portraitMode : Bool
        portraitMode =
            case model of
                Playing ( playingModel, config ) ->
                    config.portraitMode

                Ready ( readyModel, config ) ->
                    config.portraitMode

                Preparing _ ->
                    False
    in
    { title = "Factory City"
    , body =
        [ Html.node "meta"
            [ Attributes.attribute "name" "viewport"
            , Attributes.attribute "content" "width=device-width, initial-scale=1.0"
            ]
            []
        , Element.layoutWith
            { options = forceHover portraitMode ++ Framework.layoutOptions }
            ([ Background.color <| Element.rgb255 44 48 51
             ]
                ++ (maybeShade
                        |> Maybe.map
                            (\{ isWon, shade } ->
                                List.singleton <|
                                    Element.inFront <|
                                        (if isWon then
                                            Shade.viewWon

                                         else
                                            Shade.viewNormal
                                        )
                                        <|
                                            shade
                            )
                        |> Maybe.withDefault []
                   )
                ++ Framework.layoutAttributes
            )
          <|
            Element.column Framework.container <|
                content
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
