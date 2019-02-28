module LittleWorldPuzzler.View.Game exposing (view)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Modifier as Modifier exposing (Modifier(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)


viewShade : Float -> List (Attribute msg) -> List (Element msg) -> Element msg
viewShade scale attributes content =
    Element.el
        (attributes
            |> List.append
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                , Element.centerX
                , Element.centerY
                , Border.rounded (floor <| 10 * scale)
                , Font.family
                    [ Font.sansSerif ]
                ]
        )
    <|
        Element.column
            [ Element.centerX
            , Element.centerY
            , Element.spacing (floor <| 40 * scale)
            ]
        <|
            content


viewLost : { scale : Float, score : Int } -> Element msg
viewLost { scale, score } =
    viewShade scale
        [ Background.color <| Element.rgba255 0 0 0 0.7
        , Font.color <| Element.rgb255 255 255 255
        ]
        [ Element.el [ Font.size <| floor <| scale * 100, Element.centerX ] <|
            Element.text "Game Over"
        , Element.el [ Font.size <| floor <| scale * 80, Element.centerX ] <|
            Element.text "Score"
        , Element.el [ Font.size <| floor <| scale * 200, Element.centerX ] <|
            Element.text <|
                String.fromInt <|
                    score
        ]


viewNewHighscore : { scale : Float, score : Int, requestedReplayMsg : msg } -> Element msg
viewNewHighscore { scale, score, requestedReplayMsg } =
    viewShade scale
        [ Background.color <| Element.rgba255 204 166 0 0.7
        , Font.color <| Element.rgb255 0 0 0
        ]
        [ Element.el [ Font.size <| floor <| scale * 80, Element.centerX ] <|
            Element.text "New Highscore"
        , Element.el [ Font.size <| floor <| scale * 200, Element.centerX ] <|
            Element.text <|
                String.fromInt <|
                    score
        , Button.view
            [ Element.padding <| floor <| 7 * scale
            , Border.rounded (floor <| 10 * scale)
            , Font.size <| floor <| 36 * scale
            , Font.family
                [ Font.sansSerif ]
            , Element.centerX
            , Element.padding 10
            ]
          <|
            { onPress = Just requestedReplayMsg
            , label = Element.text "Replay"
            }
        ]


viewHighscore : { scale : Float, score : Int, highscore : Int, requestedReplayMsg : msg } -> Element msg
viewHighscore { scale, score, highscore, requestedReplayMsg } =
    viewShade scale
        [ Background.color <| Element.rgba255 0 0 0 0.7
        , Font.color <| Element.rgb255 255 255 255
        ]
        [ Element.el [ Font.size <| floor <| scale * 100, Element.centerX ] <|
            Element.text "Game Over"
        , Element.column [ Element.centerX ]
            [ Element.el [ Font.size <| floor <| scale * 50, Element.centerX ] <|
                Element.text "Score"
            , Element.el [ Font.size <| floor <| scale * 150, Element.centerX ] <|
                Element.text <|
                    String.fromInt <|
                        score
            , Element.el [ Font.size <| floor <| scale * 50, Element.centerX ] <|
                Element.text <|
                    "Highscore"
            , Element.el [ Font.size <| floor <| scale * 80, Element.centerX ] <|
                Element.text <|
                    String.fromInt <|
                        highscore
            ]
        , Button.view
            [ Element.padding <| floor <| 7 * scale
            , Border.rounded (floor <| 10 * scale)
            , Element.padding 10
            , Font.size <| floor <| 36 * scale
            , Font.family
                [ Font.sansSerif ]
            , Element.centerX
            , Font.color <| Element.rgb255 0 0 0
            ]
          <|
            { onPress = Just requestedReplayMsg
            , label =
                Element.text "Replay Highscore"
            }
        ]


view : { scale : Float, status : Maybe EndCondition, selected : Maybe Selected, restartMsg : msg, highscore : Maybe Int } -> Maybe { positionSelectedMsg : Position -> msg, selectedMsg : Selected -> msg, requestedReplayMsg : msg } -> Game -> Element msg
view { scale, selected, restartMsg, status, highscore } maybeMsgMapper { board, deck, score } =
    Element.column
        ([ Element.spacing (floor <| 5 * scale)
         , Background.color <| Element.rgb255 242 242 242
         , Element.padding (floor <| 20 * scale)
         , Border.rounded (floor <| 10 * scale)
         ]
            |> (case status of
                    Just endCondition ->
                        case maybeMsgMapper of
                            Just { requestedReplayMsg } ->
                                (::) <|
                                    Element.inFront <|
                                        case endCondition of
                                            Lost ->
                                                case highscore of
                                                    Just int ->
                                                        viewHighscore { scale = scale, score = score, highscore = int, requestedReplayMsg = requestedReplayMsg }

                                                    Nothing ->
                                                        viewLost { scale = scale, score = score }

                                            NewHighscore ->
                                                viewNewHighscore { scale = scale, score = score, requestedReplayMsg = requestedReplayMsg }

                            Nothing ->
                                identity

                    Nothing ->
                        identity
               )
            |> (if maybeMsgMapper == Nothing then
                    (::) <|
                        Element.inFront <|
                            Element.el
                                [ Element.width <| Element.fill
                                , Element.height <| Element.fill
                                , Border.rounded (floor <| 10 * scale)
                                , Background.color <| Element.rgb255 255 255 255
                                , Element.alpha 0.3
                                ]
                            <|
                                Element.el
                                    [ Element.centerX
                                    , Element.centerY
                                    , Font.size 80
                                    , Font.family
                                        [ Font.sansSerif ]
                                    ]
                                <|
                                    Element.text "REPLAY"

                else
                    identity
               )
        )
    <|
        case maybeMsgMapper of
            Just { positionSelectedMsg, selectedMsg } ->
                [ BoardView.view scale (Just positionSelectedMsg) board
                , DeckView.view scale (Just selectedMsg) selected deck
                ]

            Nothing ->
                [ BoardView.view scale Nothing board
                , DeckView.view scale Nothing selected deck
                ]
