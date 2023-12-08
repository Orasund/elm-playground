module Main exposing (main)

import Browser
import Card exposing (Card(..))
import Html exposing (Html)
import Html.Attributes
import Layout
import Location exposing (Location(..))
import Message exposing (Message(..))


type alias Model =
    { location : Location
    , cards : List Card
    , logs : List Message
    }


type Msg
    = Play Card


init : () -> ( Model, Cmd Msg )
init () =
    let
        location =
            TownSquare
    in
    ( { location = location
      , cards = [ Door, Letter, Death, Raven ]
      , logs =
            [ Info "Löse den Mortfall"
            , At location
            ]
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    [ model.logs
        |> List.map
            (\message ->
                Message.toString message
                    |> Layout.text []
            )
        |> Layout.column [ Layout.gap 8 ]
    , [ Html.text "Wähle eine Karte"
      , model.cards
            |> List.map
                (\card ->
                    Layout.textButton []
                        { onPress = Just (Play card)
                        , label = Card.name card
                        }
                )
            |> Layout.row [ Layout.gap 8 ]
      ]
        |> Layout.column []
    ]
        |> Layout.column
            [ Layout.gap 32
            , Html.Attributes.style "padding" "16px"
            , Html.Attributes.style "background" "linear-gradient(45deg, #7b0c0c, #7b0085)"
            , Html.Attributes.style "color" "white"
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play card ->
            ( Message.interact card model.location
                |> (\( logs, location ) ->
                        { model
                            | location = location
                            , logs =
                                model.logs
                                    ++ [ Played card ]
                                    ++ logs
                                    ++ (if location /= model.location then
                                            [ At location ]

                                        else
                                            []
                                       )
                        }
                   )
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
