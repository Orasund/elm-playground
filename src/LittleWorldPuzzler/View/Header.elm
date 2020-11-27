module LittleWorldPuzzler.View.Header exposing (view, viewWithUndo)

import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Grid as Grid
import LittleWorldPuzzler.Data exposing (devMode, gameVersion, updateName)


display : msg -> Int -> Element msg -> Element msg
display restartMsg score content =
    Element.row
        (Grid.spaceEvenly
            ++ [ Element.height <| Element.shrink
               ]
        )
    <|
        [ content
        , Element.el [ Font.size <| floor <| 42 ] <|
            Element.text <|
                String.fromInt score
        , Input.button
            (Button.simple
                ++ [ Font.family [ Font.sansSerif ] ]
            )
          <|
            { onPress = Just restartMsg
            , label = Element.text "Restart"
            }
        ]


viewWithUndo : { previousMsg : msg, nextMsg : msg, restartMsg : msg } -> Int -> Element msg
viewWithUndo { previousMsg, nextMsg, restartMsg } score =
    display restartMsg score <|
        Element.row
            (Grid.compact
                ++ [ Element.width Element.shrink ]
            )
        <|
            [ Input.button
                (Button.groupLeft
                    ++ [ Font.family [ Font.sansSerif ]
                       ]
                )
                { onPress = Just previousMsg
                , label = Element.text "<"
                }
            , Input.button
                (Button.groupRight
                    ++ [ Font.family [ Font.sansSerif ]
                       ]
                )
                { onPress = Just nextMsg
                , label = Element.text ">"
                }
            ]


view : Float -> msg -> Int -> Element msg
view scale restartMsg score =
    display restartMsg score <|
        Element.el
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.alignBottom
            , Font.color <|
                if devMode then
                    Element.rgb255 255 0 0

                else
                    Element.rgb255 255 255 255
            , Font.family
                [ Font.sansSerif ]
            ]
        <|
            Element.text ("Version 2." ++ String.fromInt gameVersion ++ ": " ++ updateName ++ " Update")
