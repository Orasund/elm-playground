module Emojidojo.Main exposing (main)

import Action
import Browser exposing (Document)
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Page.InRoom as InRoom
import Emojidojo.Page.SelectingRoom as SelectingRoom
import Emojidojo.Page.Waiting as Waiting
import Emojidojo.View.Error as Error
import Framework
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Http exposing (Error(..))


type Model
    = Waiting Waiting.Model
    | SelectingRoom SelectingRoom.Model
    | InRoom InRoom.Model


type Msg
    = WaitingSpecific Waiting.Msg
    | SelectingRoomSpecific SelectingRoom.Msg
    | InRoomSpecific InRoom.Msg


init : () -> ( Model, Cmd Msg )
init =
    Waiting.init
        >> Action.updating
        >> Action.config
        >> Action.withUpdate Waiting WaitingSpecific
        >> Action.apply


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( model, Cmd.none )
    in
    case ( msg, model ) of
        ( WaitingSpecific specificMsg, Waiting specificModel ) ->
            Waiting.update specificMsg specificModel
                |> Action.config
                |> Action.withUpdate
                    Waiting
                    WaitingSpecific
                |> Action.withTransition
                    SelectingRoom.init
                    SelectingRoom
                    SelectingRoomSpecific
                |> Action.apply

        ( SelectingRoomSpecific specificMsg, SelectingRoom specificModel ) ->
            SelectingRoom.update specificMsg specificModel
                |> Action.config
                |> Action.withUpdate SelectingRoom SelectingRoomSpecific
                |> Action.withTransition
                    InRoom.init
                    InRoom
                    InRoomSpecific
                |> Action.withExit (init ())
                |> Action.apply

        ( InRoomSpecific specificMsg, InRoom specificModel ) ->
            InRoom.update specificMsg specificModel
                |> Action.config
                |> Action.withUpdate InRoom InRoomSpecific
                |> Action.withExit (init ())
                |> Action.apply

        _ ->
            defaultCase


view : Model -> Document Msg
view model =
    let
        map :
            (msg -> Msg)
            -> { element : Element msg, error : Maybe Error, message : Maybe String }
            -> { element : Element Msg, error : Maybe Error, message : Maybe String }
        map fun out =
            { element = out.element |> Element.map fun
            , error = out.error
            , message = out.message
            }

        { element, error, message } =
            case model of
                Waiting specificModel ->
                    Waiting.view specificModel
                        |> map WaitingSpecific

                SelectingRoom specificModel ->
                    SelectingRoom.view specificModel
                        |> map SelectingRoomSpecific

                InRoom specificModel ->
                    InRoom.view specificModel
                        |> map InRoomSpecific
    in
    { title = "Emojidojo"
    , body =
        List.singleton <|
            Framework.layout [] <|
                Element.el
                    (Framework.container
                        ++ (List.singleton <|
                                Element.height <|
                                    Element.fill
                           )
                    )
                <|
                    Element.column Grid.simple <|
                        [ element
                        , case error of
                            Just err ->
                                Error.view err

                            Nothing ->
                                Element.none
                        , message
                            |> Maybe.map Element.text
                            |> Maybe.withDefault Element.none
                        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Waiting specificModel ->
            Waiting.subscriptions specificModel
                |> Sub.map WaitingSpecific

        SelectingRoom specificModel ->
            SelectingRoom.subscriptions specificModel
                |> Sub.map SelectingRoomSpecific

        InRoom specificModel ->
            InRoom.subscriptions specificModel
                |> Sub.map InRoomSpecific


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
