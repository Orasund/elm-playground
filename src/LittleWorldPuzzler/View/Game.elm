module LittleWorldPuzzler.View.Game exposing (view, viewFinished, viewHome, viewReplay)

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


viewLost : { scale : Float, score : Int, error : Maybe String } -> Element msg
viewLost { scale, score, error } =
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
        , Element.paragraph
            [ Element.alignLeft
            , Font.color <| Element.rgb 255 0 0
            , Font.size <| 14
            ]
          <|
            [ Element.text
                (error
                    |> Maybe.withDefault ""
                )
            ]
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


viewFinished : { scale : Float, status : EndCondition, highscore : Maybe Int, requestedReplayMsg : msg, error : Maybe String } -> Game -> Element msg
viewFinished { scale, status, highscore, requestedReplayMsg, error } { board, deck, score } =
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
                                        viewLost { scale = scale, score = score, error = error }

                            NewHighscore ->
                                viewNewHighscore { scale = scale, score = score, requestedReplayMsg = requestedReplayMsg }
               )
        )
    <|
        [ BoardView.view scale Nothing board
        , DeckView.view scale { sort = False } Nothing Nothing deck
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
        , DeckView.view scale { sort = False } Nothing Nothing deck
        ]


viewMode : Float -> msg -> { title : String, desc : String } -> Element msg
viewMode scale msg { title, desc } =
    Button.view
        [ Element.padding <| floor <| 10 * scale
        , Border.rounded (floor <| 10 * scale)
        , Font.size <| floor <| 36 * scale
        , Font.family
            [ Font.sansSerif ]
        , Element.centerX
        , Font.color <| Element.rgb255 0 0 0
        ]
    <|
        { onPress = Just msg
        , label =
            Element.column
                [ Element.centerX
                , Border.rounded <| floor <| scale * 10
                , Element.padding <| floor <| scale * 10
                , Element.spacing <| floor <| scale * 10
                , Element.width <| Element.px <| floor <| scale * 400
                ]
            <|
                [ Element.el [ Font.size <| floor <| scale * 40, Element.centerX ] <|
                    Element.text title
                , Element.paragraph
                    [ Font.size <| floor <| scale * 20
                    , Element.width <| Element.fill
                    ]
                  <|
                    [ Element.text
                        desc
                    ]
                ]
        }


viewHome : Float -> { normalModeSelectedMsg : msg, trainingModeSelectedMsg : msg, challengeModeSelectedMsg : msg } -> Game -> Element msg
viewHome scale { normalModeSelectedMsg, challengeModeSelectedMsg } { board, deck } =
    Element.column
        ([ Element.spacing (floor <| 5 * scale)
         , Background.color <| Element.rgb255 242 242 242
         , Element.padding (floor <| 20 * scale)
         , Border.rounded (floor <| 10 * scale)
         ]
            |> ((::) <|
                    Element.inFront <|
                        viewShade scale
                            [ Background.color <| Element.rgba255 0 0 0 0.7
                            ]
                            [ Element.row []
                                [ Element.el
                                    [ Font.size <| floor <| scale * 150
                                    , Font.family
                                        [ Font.typeface "Noto Emoji" ]
                                    ]
                                  <|
                                    Element.text "🌍"
                                , Element.column
                                    [ Font.size <| floor <| scale * 80
                                    , Element.centerX
                                    , Font.color <| Element.rgb255 255 255 255
                                    , Font.center
                                    ]
                                  <|
                                    [ Element.text "Little"
                                    , Element.text "World"
                                    , Element.text "Puzzler"
                                    ]
                                ]
                            , viewMode scale
                                normalModeSelectedMsg
                                { title = "Normal"
                                , desc = "Random cards, one life. If you loose you can blame RNG."
                                }
                            , viewMode scale
                                challengeModeSelectedMsg
                                { title = "Monthly Challenge"
                                , desc = "No randomness. Fixed card order. Comes with an undo button."
                                }
                            ]
               )
        )
    <|
        [ BoardView.view scale Nothing board
        , DeckView.view scale { sort = False } Nothing Nothing deck
        ]


view : { scale : Float, selected : Maybe Selected, sort : Bool } -> { positionSelectedMsg : Position -> msg, selectedMsg : Selected -> msg } -> Game -> Element msg
view { scale, selected, sort } { positionSelectedMsg, selectedMsg } { board, deck } =
    Element.column
        [ Element.spacing (floor <| 5 * scale)
        , Background.color <| Element.rgb255 242 242 242
        , Element.padding (floor <| 20 * scale)
        , Border.rounded (floor <| 10 * scale)
        , Element.width <| Element.fill
        ]
    <|
        [ BoardView.view scale (Just positionSelectedMsg) board
        , DeckView.view scale { sort = sort } (Just selectedMsg) selected deck
        ]
