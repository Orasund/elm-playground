module LittleWorldPuzzler.View.Header exposing (view, viewWithUndo)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import LittleWorldPuzzler.Data exposing (devMode, gameVersion, updateName)
import LittleWorldPuzzler.View.Button as Button


display : Float -> msg -> Int -> Element msg -> Element msg
display scale restartMsg score content =
    Element.row
        [ Element.spaceEvenly
        , Element.centerX
        , Element.width <| Element.px <| floor <| 608 * scale
        , Element.height <| Element.px <| floor <| scale * 52
        ]
    <|
        [ content
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


viewWithUndo : Float -> { previousMsg : msg, nextMsg : msg, restartMsg : msg } -> Int -> Element msg
viewWithUndo scale { previousMsg, nextMsg, restartMsg } score =
    display scale restartMsg score <|
        Element.row
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.spacing 5
            ]
        <|
            [ Button.view
                [ Element.padding <| floor <| 7 * scale
                , Border.rounded (floor <| 10 * scale)
                , Font.size <| floor <| 36 * scale
                , Element.width <| Element.px <| floor <| 36 * scale
                , Font.family
                    [ Font.sansSerif ]
                ]
                { onPress = Just previousMsg
                , label = Element.text "<"
                }
            , Button.view
                [ Element.padding <| floor <| 7 * scale
                , Border.rounded (floor <| 10 * scale)
                , Element.width <| Element.px <| floor <| 36 * scale
                , Font.size <| floor <| 36 * scale
                , Font.family
                    [ Font.sansSerif ]
                ]
                { onPress = Just nextMsg
                , label = Element.text ">"
                }
            ]


view : Float -> msg -> Int -> Element msg
view scale restartMsg score =
    display scale restartMsg score <|
        Element.el
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.alignBottom
            , Font.size <| floor <| 20 * scale
            , Font.color <|
                if devMode then
                    Element.rgb255 255 0 0

                else
                    Element.rgb255 255 255 255
            , Font.family
                [ Font.sansSerif ]
            ]
        <|
            Element.text ("Version " ++ String.fromInt gameVersion ++ ": " ++ updateName ++ " Update")
