module Mezzo.Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html)
import List.Extra as List
import Mezzo.Page.Ready as Ready
import Random exposing (Seed)
import Result.Extra as Result
import Task



----------------------
-- Model
----------------------


type alias SetupModel =
    { seed : Maybe Seed
    , window : Maybe { width : Int, height : Int }
    }


type Model
    = Setup SetupModel
    | Ready Ready.Model


type SetupMsg
    = GotSeed Seed
    | GotWindow { width : Int, height : Int }


type Msg
    = WhileReady Ready.Msg
    | WhileSetup SetupMsg



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init () =
    ( Setup
        { seed = Nothing
        , window = Nothing
        }
    , [ Random.independentSeed
            |> Random.generate (GotSeed >> WhileSetup)
      , Task.perform
            (\{ viewport } ->
                { height = round viewport.height
                , width = round viewport.width
                }
                    |> GotWindow
                    |> WhileSetup
            )
            Dom.getViewport
      ]
        |> Cmd.batch
    )



----------------------
-- Update
----------------------


resolve : SetupModel -> Result SetupModel Ready.Transition
resolve model =
    Maybe.map2
        (\seed window ->
            Ok
                { seed = seed
                , window = window
                }
        )
        model.seed
        model.window
        |> Maybe.withDefault (Err model)


update : Msg -> Model -> ( Model, Cmd Msg )
update mg ml =
    case ( mg, ml ) of
        ( WhileSetup msg, Setup model ) ->
            case msg of
                GotSeed seed ->
                    case { model | seed = Just seed } |> resolve of
                        Ok transition ->
                            transition
                                |> Ready.init
                                |> Tuple.mapBoth Ready (Cmd.map WhileReady)

                        Err newModel ->
                            ( Setup newModel, Cmd.none )

                GotWindow window ->
                    case { model | window = Just window } |> resolve of
                        Ok transition ->
                            transition
                                |> Ready.init
                                |> Tuple.mapBoth Ready (Cmd.map WhileReady)

                        Err newModel ->
                            ( Setup newModel, Cmd.none )

        ( WhileReady msg, Ready model ) ->
            Ready.update msg model
                |> Tuple.mapBoth Ready (Cmd.map WhileReady)

        ( _, _ ) ->
            ( ml, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions m =
    case m of
        Setup _ ->
            Sub.none

        Ready model ->
            Ready.subscriptions model
                |> Sub.map WhileReady



----------------------
-- View
----------------------


view : Model -> List (Html Msg)
view m =
    case m of
        Setup _ ->
            []

        Ready model ->
            Ready.view model
                |> Html.map WhileReady
                |> List.singleton



----------------------
-- Main
----------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "Mezzo"
                , body =
                    view model
                }
        , update = update
        , subscriptions = subscriptions
        }
