module FactoryCity.State.Finished exposing (Model, Msg(..), TransitionData, init, update, view)

import Action
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import FactoryCity.Data.Entry as Entry exposing (Entry)
import FactoryCity.Data.Game exposing (EndCondition(..), Game)
import FactoryCity.View.Game as GameView
import FactoryCity.View.Header as HeaderView
import FactoryCity.View.PageSelector as PageSelectorView
import Framework.Button as Button
import Framework.Heading as Heading
import Http exposing (Error(..))
import UndoList exposing (UndoList)


type alias TransitionData =
    { game : Game
    , history : UndoList Game
    , challenge : Bool
    }



----------------------
-- Init
----------------------


init : TransitionData -> ( Model, Cmd Msg )
init { game, history, challenge } =
    ( End
        { game = game
        , history = history
        , error = Nothing
        , challenge = challenge
        }
    , Cmd.none
    )



----------------------
-- Model
----------------------


type alias Basic =
    { game : Game
    }


type alias EndState basic =
    { basic
        | history : UndoList Game
        , challenge : Bool
        , error : Maybe Error
    }


type alias LeaderboardState basic =
    { basic
        | highscore : Entry
        , newHighscore : Bool
    }


type Model
    = End (EndState Basic)
    | Highscore (LeaderboardState Basic)


type Msg
    = RequestedReplay


type alias Action =
    Action.Action Model Msg (UndoList Game) Never



----------------------
-- Update
----------------------


update : Msg -> Model -> Action
update msg model =
    let
        defaultCase : Action
        defaultCase =
            Action.updating
                ( model, Cmd.none )
    in
    case msg of
        RequestedReplay ->
            case model of
                Highscore { highscore } ->
                    case highscore.history |> UndoList.toList of
                        present :: future ->
                            Action.transitioning <|
                                UndoList.fromList present future

                        _ ->
                            defaultCase

                _ ->
                    defaultCase



----------------------
-- View
----------------------


viewScore : { requestedReplayMsg : msg, restartMsg : msg } -> { score : Int, response : Maybe (Result Error ( Int, Bool )) } -> List (Element msg)
viewScore { requestedReplayMsg, restartMsg } { score, response } =
    List.concat
        [ [ Element.el (Heading.h2 ++ [ Element.centerX ]) <|
                Element.text <|
                    case response of
                        Just (Ok ( _, True )) ->
                            "New Highscore"

                        _ ->
                            "Game Over"
          , Element.el (Heading.h3 ++ [ Element.centerX ]) <|
                Element.text "Score"
          , Element.el (Heading.h1 ++ [ Element.centerX ]) <|
                Element.text <|
                    String.fromInt <|
                        score
          ]
        , case response of
            Just (Ok ( highscore, _ )) ->
                [ Element.el (Heading.h3 ++ [ Element.centerX ]) <|
                    Element.text <|
                        "Highscore"
                , Element.el (Heading.h4 ++ [ Element.centerX ]) <|
                    Element.text <|
                        String.fromInt <|
                            highscore
                , Input.button
                    (Button.simple
                        ++ [ Font.family [ Font.sansSerif ]
                           , Element.centerX
                           , Font.color <| Element.rgb 0 0 0
                           ]
                    )
                  <|
                    { onPress = Just requestedReplayMsg
                    , label =
                        Element.text "Replay Highscore"
                    }
                ]

            Just (Err error) ->
                List.singleton <|
                    Element.paragraph
                        [ Element.alignLeft
                        , Font.color <| Element.rgb 255 0 0
                        , Element.centerX
                        ]
                    <|
                        [ Element.text <|
                            viewError <|
                                error
                        ]

            _ ->
                []
        , [ Input.button
                (Button.simple
                    ++ [ Font.family [ Font.sansSerif ]
                       , Element.centerX
                       , Font.color <| Element.rgb 0 0 0
                       ]
                )
            <|
                { onPress = Just restartMsg
                , label =
                    Element.text "Restart"
                }
          ]
        ]


viewError : Error -> String
viewError e =
    case e of
        BadUrl string ->
            "BadUrl: " ++ string

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network Error"

        BadStatus int ->
            "Response Status: " ++ String.fromInt int

        BadBody string ->
            string


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper model =
    let
        ({ score } as game) =
            case model of
                End m ->
                    m.game

                Highscore m ->
                    m.game
    in
    ( Just
        { isWon =
            case model of
                End _ ->
                    False

                Highscore { newHighscore } ->
                    newHighscore
        , shade =
            viewScore
                { requestedReplayMsg = msgMapper RequestedReplay
                , restartMsg = restartMsg
                }
                { score = score
                , response =
                    case model of
                        End { error } ->
                            error |> Maybe.map Err

                        Highscore { highscore, newHighscore } ->
                            Just <|
                                Ok <|
                                    ( highscore.history.present.score
                                    , newHighscore
                                    )
                }
        }
    , [ HeaderView.view scale restartMsg game.score
      , GameView.viewFinished scale game
      ]
    )
