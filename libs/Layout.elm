module Layout exposing (..)

{-| elm-layout

HTML like elm-ui

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import UndoList.Decode exposing (msg)


{-|

    fill : Attribute msg
    fill =
        fillPortion 1

-}
fill : Attribute msg
fill =
    fillPortion 1


{-|

    fillPortion : Int -> Attribute msg
    fillPortion n =
        Attr.style "flex" (String.fromInt n)

-}
fillPortion : Int -> Attribute msg
fillPortion n =
    Attr.style "flex" (String.fromInt n)


{-|

    spacing : Float -> Attribute msg
    spacing n =
        Attr.style "gap" (String.fromFloat n ++ "px")

-}
spacing : Float -> Attribute msg
spacing n =
    Attr.style "gap" (String.fromFloat n ++ "px")


{-|

    noWrap : Attribute msg
    noWrap =
        Attr.style "flex-wrap" "nowrap"

-}
noWrap : Attribute msg
noWrap =
    Attr.style "flex-wrap" "nowrap"


{-|

    alignBaseline : Attribute msg
    alignBaseline =
        Attr.style "align-items" "baseline"

-}
alignBaseline : Attribute msg
alignBaseline =
    Attr.style "align-items" "baseline"


{-|

    alignCenter : Attribute msg
    alignCenter =
        Attr.style "align-items" "center"

-}
alignCenter : Attribute msg
alignCenter =
    Attr.style "align-items" "center"


{-|

    spaceBetween : Attribute msg
    spaceBetween =
        Attr.style "justify-content" "space-between"

-}
spaceBetween : Attribute msg
spaceBetween =
    Attr.style "justify-content" "space-between"


{-|

    gap : Int -> Attribute msg
    gap int =
        Attr.style "gap" (String.fromInt int ++ "px")

-}
gap : Int -> Attribute msg
gap int =
    Attr.style "gap" (String.fromInt int ++ "px")


sticky : List (Attribute msg)
sticky =
    [ Attr.style "position" "sticky"
    , Attr.style "z-index" "99999"
    ]


{-|

    stickyOnTop : List (Attribute msg)
    stickyOnTop =
        [ Attr.style "position" "sticky"
        , Attr.style "top" "0"
        , Attr.style "z-index" "99999"
        ]

-}
stickyOnTop : List (Attribute msg)
stickyOnTop =
    Attr.style "top" "0" :: sticky


{-|

    stickyOnBottom : List (Attribute msg)
    stickyOnBottom =
        [ Attr.style "position" "sticky"
        , Attr.style "bottom" "0"
        , Attr.style "z-index" "99999"
        ]

-}
stickyOnBottom : List (Attribute msg)
stickyOnBottom =
    Attr.style "bottom" "0" :: sticky


{-|

    centerContent : Attribute msg
    centerContent =
        Attr.style "justify-content" "center"

-}
centerContent : Attribute msg
centerContent =
    Attr.style "justify-content" "center"


{-|

    none : Html msg
    none =
        Html.text ""

-}
none : Html msg
none =
    Html.text ""


{-|

    el : List (Attribute msg) -> Html msg -> Html msg
    el attrs content =
        Html.div
            (Attr.style "display" "flex"
                :: attrs
            )
            [ content ]

-}
el : List (Attribute msg) -> Html msg -> Html msg
el attrs content =
    Html.div
        (Attr.style "display" "flex"
            :: attrs
        )
        [ content ]


{-|

    row : List (Attribute msg) -> List (Html msg) -> Html msg
    row attrs =
        Html.div
            ([ Attr.style "display" "flex"
             , Attr.style "flex-direction" "row"
             , Attr.style "flex-wrap" "wrap"
             ]
                ++ attrs
            )

-}
row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs =
    Html.div
        ([ Attr.style "display" "flex"
         , Attr.style "flex-direction" "row"
         , Attr.style "flex-wrap" "wrap"
         ]
            ++ attrs
        )


{-|

    column : List (Attribute msg) -> List (Html msg) -> Html msg
    column attrs =
        Html.div
            ([ Attr.style "display" "flex"
             , Attr.style "flex-direction" "column"
             ]
                ++ attrs
            )

-}
column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs =
    Html.div
        ([ Attr.style "display" "flex"
         , Attr.style "flex-direction" "column"
         ]
            ++ attrs
        )


{-| You have to set a fixed hight to see the scrollbars --> you can use 100vh for full height.
-}
sidebarContainer :
    { sidebar : Html msg
    , main : Html msg
    }
    -> Html msg
sidebarContainer args =
    Html.div
        [ --Attr.class "container"
          Attr.style "display" "flex"
        ]
        [ Html.aside
            [ --Attr.class "container__sidebar"
              Attr.style "width" "30%"

            -- Make it scrollable
            , Attr.style "overflow" "auto"
            ]
            [ args.sidebar ]
        , Html.main_
            [ --Attr.class "container__main"
              -- Take the remaining width
              Attr.style "flex" "1"

            -- Make it scrollable
            , Attr.style "overflow" "auto"
            ]
            [ args.main ]
        ]


{-| for centering just use align-left and then center on your own.
-}
menu : List (List (Html msg)) -> Html msg
menu list =
    list
        |> List.map
            (\elem ->
                elem
                    |> Html.div
                        [ --Attr.class "menu__item"
                          -- Center the content horizontally
                          Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "space-between"
                        ]
            )
        |> Html.div
            [ --Attr.class "menu"
              Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"

            -- Border
            , Attr.style "border" "1px solid rgba(0, 0, 0, 0.3)"
            , Attr.style "border-radius" "4px"
            ]


radioButtonGroup : List ( Bool, Html msg ) -> Html msg
radioButtonGroup buttons =
    buttons
        |> List.indexedMap
            (\i ( isSelected, button ) ->
                Html.label
                    ([ --Attr.class "container__label"
                       -- Center the content
                       Attr.style "align-items" "center"
                     , Attr.style "display" "inline-flex"
                     , Attr.style "padding" "8px"
                     ]
                        ++ (if i == 0 then
                                [ Attr.style "border-left" "1px solid rgba(0, 0, 0, 0.3)" ]

                            else
                                [ Attr.style "border-left" "1px solid transparent" ]
                           )
                        ++ (if isSelected then
                                [ -- For selected radio
                                  Attr.style "background-color" "#00449e"
                                , Attr.style "color" "#fff"
                                ]

                            else
                                [ -- For not selected radio
                                  Attr.style "background-color" "transparent"
                                , Attr.style "color" "#ccc"
                                ]
                           )
                    )
                    --for accesability
                    [ Html.input
                        [ Attr.type_ "radio"

                        --Attr.class "container__input"
                        , Attr.style "display" "none"
                        ]
                        []
                    , button
                    ]
            )
        |> Html.div
            [ -- Attr.class "container"
              Attr.style "display" "flex"

            -- Border
            , Attr.style "border" "ipx solid rgba(0, 0, 0, 0.3)"
            , Attr.style "border-radius" "4px"
            , Attr.style "height" "32px"
            ]


propertyItem : { name : Html msg, value : Html msg } -> Html msg
propertyItem args =
    [ Html.dt [] [ args.name ]
    , Html.dd [] [ args.value ]
    ]
        |> Html.dl
            [ --Attr.class "container"
              --Content is center horizontally
              Attr.style "align-items" "center"
            , Attr.style "display" "flex"

            --The property name will stick to the left, and the value
            --will stick to the right
            , Attr.style "justify-content" "space-between"
            , Attr.style "border-bottom" "1px solid rgba(0, 0, 0, 0.3)"

            -- Spacing
            , Attr.style "margin" "0px"
            , Attr.style "padding" "8px 0px"
            ]


chip : List (Attribute msg) -> List (Html msg) -> Html msg
chip attrs list =
    list
        |> List.map
            (\it -> Html.div [] [ it ])
        |> Html.div
            ([ --Center the content
               Attr.style "align-items" "center"
             , Attr.style "display" "inline-flex"
             , Attr.style "justify-content" "center"

             --Background color
             , Attr.style "background-color" "rgba(0, 0, 0, 0.1)"

             --Rounded border
             , Attr.style "border-radius" "999999px"

             --Spacing
             , Attr.style "padding" "4px 8px"
             , Attr.style "gap" "4px"
             ]
                ++ attrs
            )


footer : Html msg -> Html msg
footer container =
    container
        |> List.singleton
        |> Html.div
            [ Attr.style "position" "sticky"
            , Attr.style "bottom" "0"
            , Attr.style "background-color" "#fff"
            , Attr.style "max-height" "20vh"
            ]
