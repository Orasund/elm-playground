module Emojidojo.Main exposing (Game, define)

import Action
import Browser exposing (Document)
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Page.InGame as InGame
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


type alias Game data msg =
    Program () (Model data) (Msg msg)


type Model data
    = Waiting Waiting.Model
    | SelectingRoom SelectingRoom.Model
    | InRoom InRoom.Model
    | InGame (InGame.Model data)


type Msg msg
    = WaitingSpecific Waiting.Msg
    | SelectingRoomSpecific SelectingRoom.Msg
    | InRoomSpecific InRoom.Msg
    | InGameSpecific InGame.Msg
    | GameSpecific msg


init : Config -> () -> ( Model data, Cmd (Msg msg) )
init config =
    Waiting.init config
        >> Action.updating
        >> Action.config
        >> Action.withUpdate Waiting WaitingSpecific
        >> Action.apply


update : { init : data, config : Config } -> Msg msg -> Model data -> ( Model data, Cmd (Msg msg) )
update input msg model =
    case ( msg, model ) of
        ( WaitingSpecific specificMsg, Waiting specificModel ) ->
            Waiting.update input.config specificMsg specificModel
                |> Action.config
                |> Action.withUpdate
                    Waiting
                    WaitingSpecific
                |> Action.withTransition
                    (SelectingRoom.init input.config)
                    SelectingRoom
                    SelectingRoomSpecific
                |> Action.apply

        ( SelectingRoomSpecific specificMsg, SelectingRoom specificModel ) ->
            SelectingRoom.update input.config specificMsg specificModel
                |> Action.config
                |> Action.withUpdate SelectingRoom SelectingRoomSpecific
                |> Action.withTransition
                    (InRoom.init input.config)
                    InRoom
                    InRoomSpecific
                |> Action.withExit (init input.config ())
                |> Action.apply

        ( InRoomSpecific specificMsg, InRoom specificModel ) ->
            InRoom.update { init = input.init, config = input.config } specificMsg specificModel
                |> Action.config
                |> Action.withUpdate InRoom InRoomSpecific
                |> Action.withTransition
                    (InGame.init input.config)
                    InGame
                    InGameSpecific
                |> Action.withExit (init input.config ())
                |> Action.apply

        ( InGameSpecific specificMsg, InGame specificModel ) ->
            InGame.update input.config specificMsg specificModel
                |> Action.config
                |> Action.withUpdate InGame InGameSpecific
                |> Action.withExit (init input.config ())
                |> Action.apply

        _ ->
            ( model, Cmd.none )


view : { config : Config, view : data -> Element msg, title : String } -> Model data -> Document (Msg msg)
view input model =
    let
        map :
            (msg1 -> Msg msg)
            -> { element : Element msg1, error : Maybe Error, message : Maybe String }
            -> { element : Element (Msg msg), error : Maybe Error, message : Maybe String }
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
                    InRoom.view input.config specificModel
                        |> map InRoomSpecific

                InGame specificModel ->
                    InGame.view
                        { dataView = input.view >> Element.map GameSpecific
                        , msgMapper = InGameSpecific
                        }
                        specificModel
    in
    { title = input.title
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


subscriptions : Model data -> Sub (Msg msg)
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

        InGame specificModel ->
            InGame.subscriptions specificModel
                |> Sub.map InGameSpecific


define : { init : data, view : data -> Element msg, title : String, config : Config } -> Game data msg
define input =
    let
        c :
            { init : () -> ( Model data, Cmd (Msg msg) )
            , view : Model data -> Document (Msg msg)
            , update : Msg msg -> Model data -> ( Model data, Cmd (Msg msg) )
            , subscriptions : Model data -> Sub (Msg msg)
            }
        c =
            { init = init input.config
            , view =
                view
                    { config = input.config
                    , title = input.title
                    , view = input.view
                    }
            , update = update { init = input.init, config = input.config }
            , subscriptions = subscriptions
            }
    in
    Browser.document c
