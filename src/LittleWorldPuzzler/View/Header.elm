module LittleWorldPuzzler.View.Header exposing (view)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Framework.Modifier exposing (Modifier(..))
import Http exposing (Error(..))
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import LittleWorldPuzzler.Data.Entry exposing (gameVersion)
import LittleWorldPuzzler.Data.Game exposing (EndCondition(..))
import LittleWorldPuzzler.Request exposing (Response(..))
import LittleWorldPuzzler.View.Button as Button


view : Float -> msg -> Int -> Element msg
view scale restartMsg score =
    Element.row
        [ Element.spaceEvenly
        , Element.centerX
        , Element.width <| Element.px <| floor <| 608 * scale
        ]
    <|
        [ Element.el
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.alignBottom
            , Font.size <| floor <| 20 * scale
            , Font.color <| Element.rgb255 255 255 255
            , Font.family
                [ Font.sansSerif ]
            ]
          <|
            Element.text ("Version " ++ String.fromInt gameVersion ++ ": Sea Update")
        , Element.el [ Font.size <| floor <| 50 * scale ] <|
            Element.text <|
                String.fromInt score
        , Button.view
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.padding <| floor <| 7 * scale
            , Border.rounded (floor <| 10 * scale)
            , Font.size <| floor <| 36 * scale
            , Font.family
                [ Font.sansSerif ]
            ]
          <|
            { onPress = Just restartMsg
            , label = Element.text "Restart"
            }
        ]
