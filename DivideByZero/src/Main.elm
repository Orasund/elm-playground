module Main exposing (main)

import Array
import Browser
import Expression exposing (Expression(..), Operator(..), Symbol)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level
import View


type alias Model =
    { game : Game
    }


type Msg
    = InputPressed Symbol
    | DeletePressed


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        level =
            Array.get model.game.level Level.levels
                |> Maybe.withDefault Level.errorLevel

        gameWon =
            model.game.expression == DivisionByZero
    in
    [ View.stylesheet
    , [ model.game.expression
            |> Expression.toString
            |> Html.text
            |> Layout.el [ Html.Attributes.id "screen" ]
      , [ "Level " ++ String.fromInt model.game.level |> Layout.text []
        , "Goal: " ++ String.fromInt level.goal |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
        ]
            |> Html.div [ Html.Attributes.class "info-row" ]
      , Layout.textButton [ Html.Attributes.class "primary" ]
            { label = "DEL / CLR"
            , onPress =
                DeletePressed
                    |> Just
            }
            :: (level.inputs
                    |> List.map
                        (\input ->
                            Layout.textButton []
                                { label = View.viewInput model.game input
                                , onPress =
                                    input
                                        |> InputPressed
                                        |> Just
                                }
                        )
               )
            |> Html.div [ Html.Attributes.class "button-row" ]
      ]
        |> Layout.column [ Html.Attributes.id "container" ]
    , [ "Divide by Zero" |> Layout.text [ Html.Attributes.style "font-size" "4em", Html.Attributes.style "text-align" "center" ]
      , "a game by Lucas Payr" |> Layout.text [ Html.Attributes.style "font-size" "1.5em" ]
      , [ "You found" |> Layout.text []
        , "0" |> Layout.text [ Html.Attributes.style "font-size" "4em" ]
        , "secret levels." |> Layout.text []
        ]
            |> Layout.column [ Html.Attributes.class "column" ]
      ]
        |> Layout.column
            (Html.Attributes.id "overlay"
                :: (if gameWon then
                        [ Html.Attributes.class "game-won" ]

                    else
                        []
                   )
                ++ Layout.centered
            )
    ]
        |> Html.div
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "position" "relative"
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputPressed input ->
            ( { model
                | game = model.game |> Game.addSymbol input
              }
            , Cmd.none
            )

        DeletePressed ->
            ( { model | game = model.game |> Game.deleteInput }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
