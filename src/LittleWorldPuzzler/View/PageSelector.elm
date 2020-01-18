module LittleWorldPuzzler.View.PageSelector exposing (viewCollection, viewGame)

import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Color as Color
import Framework.Grid as Grid


activeButton : msg -> String -> Element msg
activeButton msg label =
    Input.button (Button.groupBottom ++ [ Font.family [ Font.sansSerif ] ])
        { onPress = Just msg
        , label = Element.text <| label
        }


inactiveButton : String -> Element msg
inactiveButton label =
    Input.button (Button.groupBottom ++ Color.primary ++ [ Font.family [ Font.sansSerif ] ])
        { onPress = Nothing
        , label = Element.text <| label
        }


viewGame : msg -> Element msg
viewGame msg =
    Element.row (Grid.simple ++ [ Element.centerX, Element.width <| Element.shrink ])
        [ inactiveButton "Game"
        , activeButton msg "Collection"
        ]


viewCollection : msg -> Element msg
viewCollection msg =
    Element.row (Grid.simple ++ [ Element.centerX, Element.width <| Element.shrink ])
        [ activeButton msg "Game"
        , inactiveButton "Collection"
        ]
