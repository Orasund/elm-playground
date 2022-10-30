module  View.Modal exposing (..)

import Element
import Element.Font as Font
import Widget exposing (Button, Item)
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography


view : { title : String, button : Button msg, content : List (Item msg) } -> List (Element.Attribute msg)
view args =
    { content =
        (args.title
            |> Element.text
            |> Element.el
                (Typography.h6
                    ++ [ Element.centerX
                       , Element.centerY
                       ]
                )
            |> Widget.asItem
        )
            :: args.content
            ++ [ Widget.button
                    (Material.containedButton Material.defaultPalette
                        |> Customize.elementButton [ Element.centerX ]
                    )
                    args.button
                    |> Widget.asItem
               ]
            |> Widget.itemList
                (Material.cardColumn Material.defaultPalette
                    |> Customize.elementColumn
                        [ Element.centerY
                        , Element.width <| Element.px 250
                        , Element.centerX
                        , Font.family [ Font.serif ]
                        ]
                    |> Customize.mapContent
                        (Customize.element [ Element.width <| Element.px 250 ])
                )
    , onDismiss = Nothing
    }
        |> List.singleton
        |> Widget.singleModal
