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
import Jsonstore exposing (Json)


type alias Game data remote msg =
    Program () (Model remote data) (Msg data remote msg)


type Model remote data
    = Waiting (Waiting.Model remote)
    | SelectingRoom (SelectingRoom.Model remote)
    | InRoom InRoom.Model
    | InGame (InGame.Model data)


type Msg data remote msg
    = WaitingSpecific (Waiting.Msg remote)
    | SelectingRoomSpecific (SelectingRoom.Msg remote)
    | InRoomSpecific (InRoom.Msg remote)
    | InGameSpecific (InGame.Msg remote msg)


init : Json remote -> Config -> () -> ( Model remote data, Cmd (Msg data remote msg) )
init jsonData config =
    Waiting.init jsonData config
        >> Action.updating
        >> Action.config
        >> Action.withUpdate Waiting WaitingSpecific
        >> Action.apply


update :
    { init : remote -> data
    , config : Config
    , update : msg -> data -> ( data, Cmd msg )
    , json : Json remote
    , remoteWrapper : remote -> msg
    , remoteInit : remote
    , remoteFromModel : data -> remote
    }
    -> Msg data remote msg
    -> Model remote data
    -> ( Model remote data, Cmd (Msg data remote msg) )
update input msg model =
    case ( msg, model ) of
        ( WaitingSpecific specificMsg, Waiting specificModel ) ->
            Waiting.update input.json input.config specificMsg specificModel
                |> Action.config
                |> Action.withUpdate
                    Waiting
                    WaitingSpecific
                |> Action.withTransition
                    (SelectingRoom.init input.json input.config)
                    SelectingRoom
                    SelectingRoomSpecific
                |> Action.apply

        ( SelectingRoomSpecific specificMsg, SelectingRoom specificModel ) ->
            SelectingRoom.update input.json input.config specificMsg specificModel
                |> Action.config
                |> Action.withUpdate SelectingRoom SelectingRoomSpecific
                |> Action.withTransition
                    (InRoom.init input.config)
                    InRoom
                    InRoomSpecific
                |> Action.withExit (init input.json input.config ())
                |> Action.apply

        ( InRoomSpecific specificMsg, InRoom specificModel ) ->
            InRoom.update
                { init = input.init
                , config = input.config
                , json = input.json
                , remoteInit = input.remoteInit
                }
                specificMsg
                specificModel
                |> Action.config
                |> Action.withUpdate InRoom InRoomSpecific
                |> Action.withTransition
                    (InGame.init input.json input.config)
                    InGame
                    InGameSpecific
                |> Action.withExit (init input.json input.config ())
                |> Action.apply

        ( InGameSpecific specificMsg, InGame specificModel ) ->
            InGame.update
                { json = input.json
                , update = input.update
                , config = input.config
                , remoteWrapper = input.remoteWrapper
                , remoteFromModel = input.remoteFromModel
                , init = input.init
                }
                specificMsg
                specificModel
                |> Action.config
                |> Action.withUpdate InGame InGameSpecific
                |> Action.withExit (init input.json input.config ())
                |> Action.apply

        _ ->
            ( model, Cmd.none )


view : { config : Config, view : data -> Element msg, title : String } -> Model remote data -> Document (Msg data remote msg)
view input model =
    let
        map :
            (msg1 -> Msg data remote msg)
            -> { element : Element msg1, error : Maybe Error, message : Maybe String }
            -> { element : Element (Msg data remote msg), error : Maybe Error, message : Maybe String }
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
                        { dataView =
                            input.view
                                >> Element.map
                                    (InGameSpecific << InGame.GameSpecific)
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


subscriptions : (data -> Sub msg) -> Model remote data -> Sub (Msg data remote msg)
subscriptions fun model =
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
            InGame.subscriptions fun specificModel
                |> Sub.map InGameSpecific


define :
    { init : remote -> data
    , remote :
        { msg : remote -> msg
        , json : Json remote
        , init : remote
        , fromModel : data -> remote
        }
    , view : data -> Element msg
    , subscriptions : data -> Sub msg
    , update : msg -> data -> ( data, Cmd msg )
    , title : String
    , config : Config
    }
    -> Game data remote msg
define input =
    let
        c :
            { init : () -> ( Model remote data, Cmd (Msg data remote msg) )
            , view : Model remote data -> Document (Msg data remote msg)
            , update : Msg data remote msg -> Model remote data -> ( Model remote data, Cmd (Msg data remote msg) )
            , subscriptions : Model remote data -> Sub (Msg data remote msg)
            }
        c =
            { init = init input.remote.json input.config
            , view =
                view
                    { config = input.config
                    , title = input.title
                    , view = input.view
                    }
            , update =
                update
                    { init = input.init
                    , config = input.config
                    , update = input.update
                    , json = input.remote.json
                    , remoteWrapper = input.remote.msg
                    , remoteInit = input.remote.init
                    , remoteFromModel = input.remote.fromModel
                    }
            , subscriptions = subscriptions input.subscriptions
            }
    in
    Browser.document c
