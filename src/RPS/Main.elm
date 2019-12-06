module RPS exposing (main)

import Element exposing (Element)
import Element.Input as Input
import Emojidojo exposing (Game)
import Framework.Button as Button
import Framework.Grid as Grid
import Jsonstore exposing (Json)


type alias Model =
    Int


type Msg
    = Increase
    | Decrease


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( model + 1, Cmd.none )

        Decrease ->
            ( model - 1, Cmd.none )


view : Model -> Element Msg
view model =
    Element.column Grid.simple <|
        [ Input.button Button.simple <|
            { onPress = Just Increase
            , label = Element.text <| "Increase"
            }
        , Element.text <| String.fromInt <| model
        , Input.button Button.simple <|
            { onPress = Just Decrease
            , label = Element.text <| "Decrease"
            }
        ]


main : Game Model Msg
main =
    Emojidojo.define
        { title = "Rock Paper Scissors"
        , init = 0
        , view = view
        , subscriptions =
            \_ -> Sub.none
        , update = update
        , config =
            Emojidojo.config
                { jsonstoreId = "41bda26d78d87269a31efaa657f4b9e12f39748413eb64513e4b1c50f95ed1bd"
                , version = 0.1001
                }
        , json =
            Jsonstore.int
        }
