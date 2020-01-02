module View exposing (dropDown)

import Element exposing (Attribute, Element)
import Element.Events as Events
import Element.Input as Input
import Framework.Grid as Grid


dropDown : List (Attribute msg) -> { onToggle : Bool -> msg, isDropped : Bool, label : Element msg, content : List (Element msg) } -> Element msg
dropDown attributes { onToggle, isDropped, label, content } =
    Element.el
        ([ Events.onClick <| onToggle <| not isDropped
         , Events.onLoseFocus <| onToggle False
         ]
            ++ (if isDropped then
                    [ Element.below <|
                        Element.row
                            [ Element.height <| Element.shrink
                            ]
                        <|
                            content
                    ]

                else
                    []
                        ++ attributes
               )
        )
    <|
        label


menu :
    List (Attribute msg)
    ->
        { selected : option
        , onSelect : option -> msg
        , options : List ( option, String )
        , buttonAttribute : Bool -> List (Attribute msg)
        }
    -> Element msg
menu attributes { selected, onSelect, options, buttonAttribute } =
    Element.row (Grid.spaceEvenly ++ attributes) <|
        (options
            |> List.map
                (\( option, string ) ->
                    Input.button (buttonAttribute (option == selected))
                        { onPress =
                            if option == selected then
                                Nothing

                            else
                                Just <| onSelect option
                        , label = Element.text <| string
                        }
                )
        )
