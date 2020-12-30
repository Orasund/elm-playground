module LittleWorldPuzzler.State.Finished exposing (Model, Msg(..), TransitionData, init, update, view)

import Action
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Firestore exposing (Error(..))
import Framework.Button as Button
import Framework.Heading as Heading
import Http
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)
import LittleWorldPuzzler.Data.Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.Request as Request exposing (Response(..))
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
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
    , Request.getHighscore { score = game.score, challenge = challenge }
        |> Cmd.map RequestedHighscore
    )



----------------------
-- Model
----------------------


type alias EndState =
    { game : Game
    , history : UndoList Game
    , challenge : Bool
    , error : Maybe Error
    }


type alias LeaderboardState =
    { game : Game
    , highscore : Entry
    , newHighscore : Bool
    , error : Maybe Error
    }


type Model
    = End EndState
    | Highscore LeaderboardState


type Msg
    = RequestedHighscore Response


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
        RequestedHighscore response ->
            case model of
                End ({ history, game, challenge } as endState) ->
                    case response of
                        GotHighscore entry ->
                            Action.updating
                                ( Highscore
                                    { game = game
                                    , highscore = entry
                                    , newHighscore = False
                                    , error = Nothing
                                    }
                                , Cmd.none
                                )

                        AchievedNewHighscore ->
                            let
                                newEntry : Entry
                                newEntry =
                                    Entry.new history
                            in
                            Action.updating
                                ( Highscore
                                    { game = game
                                    , highscore = newEntry
                                    , newHighscore = True
                                    , error = Nothing
                                    }
                                , Request.setHighscore { entry = newEntry, challenge = challenge }
                                    |> Cmd.map RequestedHighscore
                                )

                        GotError error ->
                            Action.updating
                                ( End
                                    { endState | error = Just error }
                                , Cmd.none
                                )

                        Done ->
                            defaultCase

                Highscore highscoreState ->
                    case response of
                        GotError error ->
                            Action.updating
                                ( Highscore
                                    { highscoreState | error = Just error }
                                , Cmd.none
                                )

                        _ ->
                            defaultCase



----------------------
-- View
----------------------


viewScore : { restartMsg : msg } -> { score : Int, response : Maybe (Result Error ( Int, Bool )) } -> List (Element msg)
viewScore { restartMsg } { score, response } =
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
        Http_ err ->
            case err of
                Http.BadUrl string ->
                    "BadUrl: " ++ string

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network Error"

                Http.BadStatus int ->
                    "Response Status: " ++ String.fromInt int

                Http.BadBody string ->
                    string

        Response err ->
            err.message


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg _ model =
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
                { restartMsg = restartMsg
                }
                { score = score
                , response =
                    case model of
                        End { error } ->
                            error |> Maybe.map Err

                        Highscore { highscore, newHighscore, error } ->
                            case error of
                                Just err ->
                                    Just (Err err)

                                Nothing ->
                                    Just <|
                                        Ok <|
                                            ( highscore.score
                                            , newHighscore
                                            )
                }
        }
    , [ HeaderView.view scale restartMsg game.score
      , GameView.viewFinished scale game
      ]
    )
