module FactoryCity.Main exposing (main)

import Action
import Browser
import Browser.Events exposing (onResize)
import Element
import Element.Background as Background
import FactoryCity.State.Playing as PlayingState
import FactoryCity.State.Prepairing as PreparingState
import FactoryCity.State.Ready as ReadyState
import Framework



----------------------
-- Model
----------------------


type alias Config =
    Float


type Model
    = Preparing PreparingState.Model
    | Ready ( ReadyState.Model, Config )
    | Playing ( PlayingState.Model, Config )


type Msg
    = PlayingSpecific PlayingState.Msg
    | ReadySpecific ReadyState.Msg
    | PreparingSpecific PreparingState.Msg
    | Resized Float


calcScale : { height : Float, width : Float } -> Float
calcScale dim =
    dim.width / 400 |> clamp 0.1 1



{--if dim.width / dim.height > width / height then
        dim.height / height

    else
        dim.width / width--}
----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    PreparingState.init
        |> Tuple.mapBoth Preparing (Cmd.map PreparingSpecific)



----------------------
-- Update
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PreparingSpecific preparingMsg, Preparing preparingModel ) ->
            PreparingState.update calcScale preparingMsg preparingModel
                |> Action.config
                |> Action.withUpdate Preparing never
                |> Action.withTransition
                    (\{ scale, shop, seed } ->
                        ReadyState.init
                            { shop = shop
                            , seed = seed
                            }
                            |> Tuple.mapFirst (\m -> ( m, scale ))
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

        ( Resized scale, _ ) ->
            ( case model of
                Playing ( playingModel, _ ) ->
                    Playing
                        ( playingModel
                        , scale
                        )

                Ready ( readyModel, _ ) ->
                    Ready
                        ( readyModel
                        , scale
                        )

                Preparing _ ->
                    model
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
        (List.concat
            [ [ onResize
                    (\w h ->
                        { width = toFloat w, height = toFloat h }
                            |> calcScale
                            |> Resized
                    )
              ]
            , case model of
                Playing playingModel ->
                    [ Sub.map PlayingSpecific <|
                        PlayingState.subscriptions <|
                            Tuple.first <|
                                playingModel
                    ]

                _ ->
                    []
            ]
        )



----------------------
-- View
----------------------


view : Model -> Browser.Document Msg
view model =
    let
        ( maybeInfrontContent, content ) =
            case model of
                Playing ( playingModel, scale ) ->
                    PlayingState.view scale PlayingSpecific playingModel

                Ready ( readyModel, scale ) ->
                    ReadyState.view scale ReadySpecific readyModel

                Preparing preparingModel ->
                    PreparingState.view preparingModel
    in
    { title = "Factory City"
    , body =
        [ {--Html.node "meta"
            [ Attributes.attribute "name" "viewport"

            , Attributes.attribute "content" "width=device-width, initial-scale=1.0, user-scalable=no"
            ]
            []
        , --}
          Element.layoutWith
            { options = Framework.layoutOptions }
            (List.concat
                [ [ Background.color <| Element.rgb255 44 48 51
                  ]
                , maybeInfrontContent
                    |> Maybe.map
                        (\( topContent, bottomContent ) ->
                            [ topContent
                                |> Element.inFront
                            , bottomContent
                                |> Element.inFront
                            ]
                        )
                    |> Maybe.withDefault []
                , Framework.layoutAttributes
                ]
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
