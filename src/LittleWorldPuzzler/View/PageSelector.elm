module LittleWorldPuzzler.View.PageSelector exposing (viewCollection, viewGame, viewInactive)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import LittleWorldPuzzler.View.Button as Button


attributes : Float -> List (Attribute msg)
attributes scale =
    [ Element.width <| Element.px <| floor <| scale * 200
    , Element.padding <| floor <| 7 * scale
    , Element.alignTop
    , Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = floor <| 10 * scale
        , bottomRight = floor <| 10 * scale
        }
    , Font.size <| floor <| 36 * scale
    , Font.family
        [ Font.sansSerif ]
    ]


activeButton : Float -> msg -> String -> Element msg
activeButton scale msg label =
    Button.view (attributes scale)
        { onPress = Just msg
        , label = Element.text <| label
        }


inactiveButton : Float -> String -> Element msg
inactiveButton scale label =
    Button.view
        ([ Background.color <| Element.rgb255 242 242 242
         , Element.height <| Element.px <| floor <| scale * 52
         , Border.width 0
         ]
            |> List.append (attributes scale)
        )
        { onPress = Nothing
        , label = Element.text <| label
        }


view : Float -> List (Element msg) -> Element msg
view scale =
    Element.row
        [ Element.height <| Element.px <| floor <| scale * 52
        , Element.centerX
        , Element.spacing <| floor <| scale * 20
        ]


viewGame : Float -> msg -> Element msg
viewGame scale msg =
    view scale <|
        [ inactiveButton scale "Game"
        , activeButton scale msg "Collection"
        ]


viewCollection : Float -> msg -> Element msg
viewCollection scale msg =
    view scale <|
        [ activeButton scale msg "Game"
        , inactiveButton scale "Collection"
        ]


viewInactive : Float -> Element msg
viewInactive scale =
    view scale <|
        []
