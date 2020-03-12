module View exposing (dropDownContent, multiSelect, select)

import Element exposing (Attribute, Element)
import Element.Events as Events
import Element.Input as Input
import Set exposing (Set)


select :
    List (Attribute msg)
    ->
        { selected : Maybe a
        , options : List a
        , label : a -> Bool -> Element msg
        , onChange : a -> msg
        }
    -> List (Element msg)
select attributes { selected, options, label, onChange } =
    options
        |> List.map
            (\a ->
                Input.button attributes
                    { onPress = a |> onChange |> Just
                    , label = label a (selected == Just a)
                    }
            )


multiSelect :
    List (Attribute msg)
    ->
        { selected : Set comparable
        , options : List comparable
        , label : comparable -> Bool -> Element msg
        , onChange : comparable -> msg
        }
    -> List (Element msg)
multiSelect attributes { selected, options, label, onChange } =
    options
        |> List.map
            (\a ->
                Input.button attributes
                    { onPress = a |> onChange |> Just
                    , label =
                        label a
                            (selected |> Set.member a)
                    }
            )


dropDownContent :
    List (Attribute msg)
    ->
        { onToggle : Bool -> msg
        , isDropped : Bool
        , label : Element msg
        , content : Element msg
        }
    -> Element msg
dropDownContent attributes { onToggle, isDropped, label, content } =
    Element.el
        ([ Events.onClick <| onToggle <| not isDropped
         , Events.onLoseFocus <| onToggle False
         ]
            ++ (if isDropped then
                    [ Element.below <| content
                    ]

                else
                    []
               )
            ++ attributes
        )
    <|
        label
