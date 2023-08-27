module Main exposing (main)

import Array
import Browser
import Expression exposing (Expression(..), Operator(..), Symbol(..))
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import View


type alias Model =
    { game : Game
    , levelSelect : Maybe { min : Int }
    , disabled : List Symbol
    }


type Msg
    = InputPressed Symbol
    | DeletePressed
    | LevelSelectPressed
    | LoadLevelPressed Int


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , levelSelect = Nothing
      , disabled = []
      }
    , Cmd.none
    )


viewButtons : { won : Bool, level : Level } -> Model -> Html Msg
viewButtons args model =
    let
        noButton =
            Html.div [ Html.Attributes.class "no-button" ] []

        placeholder =
            Layout.textButton
                [ Html.Attributes.disabled True
                ]
                { label = ""
                , onPress = Nothing
                }
    in
    (case model.levelSelect of
        Nothing ->
            [ Layout.textButton
                [ Html.Attributes.class "secondary"
                , Html.Attributes.disabled args.won
                ]
                { label = "LVL"
                , onPress =
                    LevelSelectPressed
                        |> Just
                }
            , noButton
            , Layout.textButton
                [ Html.Attributes.class "primary"
                , Html.Attributes.disabled args.won
                ]
                { label = "DEL"
                , onPress =
                    DeletePressed
                        |> Just
                }
            ]
                ++ (args.level.inputs
                        |> List.map
                            (\input ->
                                Layout.textButton
                                    [ Html.Attributes.disabled (List.member input model.disabled || args.won) ]
                                    { label = View.viewInput model.game input
                                    , onPress =
                                        input
                                            |> InputPressed
                                            |> Just
                                    }
                            )
                   )
                ++ (placeholder
                        |> List.repeat (6 - List.length args.level.inputs)
                   )
                ++ [ Layout.textButton
                        [ Html.Attributes.disabled
                            ((args.level.withVar && List.member VarSymbol model.disabled)
                                || (not args.level.withVar && model.game.var == Nothing)
                                || args.won
                            )
                        ]
                        { label = View.viewInput model.game VarSymbol
                        , onPress =
                            VarSymbol
                                |> InputPressed
                                |> Just
                        }
                   , noButton
                   , Layout.textButton
                        [ Html.Attributes.class "secondary"
                        , Html.Attributes.disabled (not args.won)
                        ]
                        { label = "NEXT"
                        , onPress =
                            LoadLevelPressed (model.game.level + 1)
                                |> Just
                        }
                   ]

        Just { min } ->
            [ Layout.textButton [ Html.Attributes.class "secondary" ]
                { label = "BACK"
                , onPress =
                    LevelSelectPressed
                        |> Just
                }
            , Html.div [ Html.Attributes.class "no-button" ] []
            , Html.div [ Html.Attributes.class "no-button" ] []
            ]
                ++ (List.range min (min + 8)
                        |> List.map
                            (\i ->
                                Layout.textButton []
                                    { label = String.fromInt i
                                    , onPress =
                                        LoadLevelPressed i
                                            |> Just
                                    }
                            )
                   )
    )
        |> Html.div [ Html.Attributes.class "button-row" ]


view : Model -> Html Msg
view model =
    let
        level =
            Array.get model.game.level Level.levels
                |> Maybe.withDefault Level.errorLevel

        gameWon =
            model.game.expression == DivisionByZero

        levelWon =
            model.game.expression == level.goal
    in
    [ View.stylesheet
    , [ (if model.levelSelect == Nothing then
            model.game.expression
                |> Expression.toString

         else
            "LEVEL = "
        )
            |> Html.text
            |> Layout.el [ Html.Attributes.id "screen" ]
      , [ "Level " ++ String.fromInt model.game.level |> Layout.text []
        , "Goal: " ++ Expression.toString level.goal |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
        ]
            |> Html.div [ Html.Attributes.class "info-row" ]
      , viewButtons
            { level = level
            , won = levelWon
            }
            model
      ]
        |> Layout.column
            (Html.Attributes.id "container"
                :: (if gameWon then
                        [ Html.Attributes.class "shaking" ]

                    else
                        []
                   )
            )
    , View.overlay { gameWon = gameWon }
    ]
        |> Layout.column
            ([ Html.Attributes.style "width" "100%"
             , Html.Attributes.style "height" "100%"
             , Html.Attributes.style "position" "relative"
             ]
                ++ Layout.centered
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputPressed input ->
            ( { model
                | game = model.game |> Game.addSymbol input
                , disabled =
                    if input == VarSymbol && model.game.var == Nothing then
                        model.disabled

                    else
                        input :: model.disabled
              }
            , Cmd.none
            )

        DeletePressed ->
            ( model.game
                |> Game.deleteInput
                |> (\game ->
                        { model
                            | game = game
                            , disabled =
                                if game.expression == Number 0 then
                                    []

                                else
                                    model.disabled
                        }
                   )
            , Cmd.none
            )

        LevelSelectPressed ->
            if model.levelSelect == Nothing then
                ( { model | levelSelect = Just { min = 1 } }
                , Cmd.none
                )

            else
                ( { model | levelSelect = Nothing }
                , Cmd.none
                )

        LoadLevelPressed level ->
            ( { model
                | levelSelect = Nothing
                , game = model.game |> Game.loadLevel level
                , disabled = []
              }
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
