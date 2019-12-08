module RPS exposing (main)

import Element exposing (Element)
import Element.Input as Input
import Emojidojo exposing (Game)
import Framework.Button as Button
import Framework.Grid as Grid
import Jsonstore exposing (Json)


type alias Model =
    { counter : Int
    , oldCounter : Int
    }


type alias RemoteData =
    Int


type Msg
    = Increase
    | Decrease
    | GotRemoteData RemoteData


init : RemoteData -> Model
init counter =
    { counter = counter
    , oldCounter = counter
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )

        Decrease ->
            ( { model | counter = model.counter - 1 }
            , Cmd.none
            )

        GotRemoteData remoteData ->
            ( { model | oldCounter = remoteData }
            , Cmd.none
            )


view : Model -> Element Msg
view model =
    Element.column Grid.simple <|
        [ Input.button Button.simple <|
            { onPress = Just Increase
            , label = Element.text <| "Increase"
            }
        , Element.text <|
            (String.fromInt <| model.counter)
                ++ " (current: "
                ++ (String.fromInt <| model.oldCounter)
                ++ ")"
        , Input.button Button.simple <|
            { onPress = Just Decrease
            , label = Element.text <| "Decrease"
            }
        ]


main : Game Model RemoteData Msg
main =
    Emojidojo.define
        { title = "Rock Paper Scissors"
        , init = init
        , view = view
        , subscriptions =
            \_ -> Sub.none
        , update = update
        , config =
            Emojidojo.config
                { jsonstoreId = "41bda26d78d87269a31efaa657f4b9e12f39748413eb64513e4b1c50f95ed1bd"
                , version = 0.1001
                }
        , remote =
            { msg = GotRemoteData
            , json = Jsonstore.int
            , init = 0
            , fromModel = .counter
            }
        }
