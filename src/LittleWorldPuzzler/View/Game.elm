module LittleWorldPuzzler.View.Game exposing (view, viewFinished, viewReplay)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Modifier exposing (Modifier(..))
import Grid.Position exposing (Position)
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import LittleWorldPuzzler.Data.Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView


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


viewFinished : { scale : Float, status : EndCondition, highscore : Maybe Int, requestedReplayMsg : msg } -> Game -> Element msg
viewFinished { scale, status, highscore, requestedReplayMsg } { board, deck, score } =
    Element.column
        ([ Element.spacing (floor <| 5 * scale)
         , Background.color <| Element.rgb255 242 242 242
         , Element.padding (floor <| 20 * scale)
         , Border.rounded (floor <| 10 * scale)
         ]
            |> ((::) <|
                    Element.inFront <|
                        case status of
                            Lost ->
                                case highscore of
                                    Just int ->
                                        viewHighscore { scale = scale, score = score, highscore = int, requestedReplayMsg = requestedReplayMsg }

                                    Nothing ->
                                        viewLost { scale = scale, score = score }

                            NewHighscore ->
                                viewNewHighscore { scale = scale, score = score, requestedReplayMsg = requestedReplayMsg }
               )
        )
    <|
        [ BoardView.view scale Nothing board
        , DeckView.view scale Nothing Nothing deck
        ]


viewReplay : Float -> Game -> Element msg
viewReplay scale { board, deck } =
    Element.column
        ([ Element.spacing (floor <| 5 * scale)
         , Background.color <| Element.rgb255 242 242 242
         , Element.padding (floor <| 20 * scale)
         , Border.rounded (floor <| 10 * scale)
         ]
            |> ((::) <|
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
               )
        )
    <|
        [ BoardView.view scale Nothing board
        , DeckView.view scale Nothing Nothing deck
        ]


view : { scale : Float, selected : Maybe Selected } -> { positionSelectedMsg : Position -> msg, selectedMsg : Selected -> msg } -> Game -> Element msg
view { scale, selected } { positionSelectedMsg, selectedMsg } { board, deck } =
    Element.column
        [ Element.spacing (floor <| 5 * scale)
        , Background.color <| Element.rgb255 242 242 242
        , Element.padding (floor <| 20 * scale)
        , Border.rounded (floor <| 10 * scale)
        ]
    <|
        [ BoardView.view scale (Just positionSelectedMsg) board
        , DeckView.view scale (Just selectedMsg) selected deck
        ]
