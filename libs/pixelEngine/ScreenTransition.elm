module PixelEngine.ScreenTransition exposing (Transition, apply, customTransition)

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import PixelEngine.Graphics as Graphics exposing (Area)


type Transition
    = Transition (List ( Float, String ))


customTransition : List ( Float, String ) -> Transition
customTransition =
    Transition


apply :
    { width : Float, scale : Float, transitionSpeedInSec : Float }
    -> { from : List (Area msg), to : List (Area msg) }
    -> { name : String, transition : Transition }
    -> Html msg
apply ({ width } as options) { from, to } { name, transition } =
    let
        transitionList : List ( Float, String )
        transitionList =
            case transition of
                Transition transitionlist ->
                    transitionlist

        transitionLength : Float
        transitionLength =
            transitionList
                |> List.map Tuple.first
                |> List.sum

        animationCss =
            transitionList
                |> List.foldl
                    (\( length, css ) ( sum, string ) ->
                        ( sum + length
                        , string
                            ++ (toString <| (sum + length) * 100 / transitionLength)
                            ++ "% {"
                            ++ "visibility:visible;"
                            ++ css
                            ++ "} "
                        )
                    )
                    ( 0, "" )
                |> Tuple.second
                |> (\a -> a ++ ";")
    in
    div
        [ css
            [ Css.backgroundColor (Css.rgb 0 0 0)
            ]
        ]
        [ Foreign.global
            [ Foreign.selector
                ("@keyframes pixelengine_screen_transition_"
                    ++ name
                )
                [ Css.property
                    animationCss
                    ""
                ]
            ]
        , div
            [ css
                [ Css.position Css.relative
                , Css.width <| px <| width
                , Css.margin Css.auto
                ]
            ]
            [ div
                []
                [ Graphics.render options to ]
            , div
                [ css
                    [ Css.position Css.absolute
                    , Css.top <| px 0
                    , Css.left <| px 0
                    , Css.visibility Css.hidden
                    , Css.property "animation"
                        ("pixelengine_screen_transition_"
                            ++ name
                            ++ " "
                            ++ toString transitionLength
                            ++ "s 1"
                        )
                    ]
                ]
                [ Graphics.render options from ]
            ]
        ]
