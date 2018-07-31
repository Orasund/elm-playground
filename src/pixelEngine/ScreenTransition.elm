module PixelEngine.ScreenTransition exposing (apply)

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import PixelEngine.Graphics as Graphics exposing (Area)


{- transition :
       String
       -> List ( Float, List ( String, String ) )
       -> { animationCss : String, transitionLength : Float, animationName : String }
   transition animationName listOfTransitions =
       let
           transitionLength =
               listOfTransitions
                   |> List.map Tuple.first
                   |> List.sum
       in
       { animationName = animationName
       , transitionLength = transitionLength
       , animationCss =
           listOfTransitions
           |> List.foldl
               (\( length, listOfFilter ) ( sum, css ) ->
                   let
                       currentTime = sum + length
                   in
                   ( currentTime
                   , 100
                   )
               )
               ( 0,
                   ((transitionLength *100)/currentTime ++ "% {"
                   ++ (listOfFilter
                       |> List.foldl
                           (\(name,value)->string->
                           string++)
                           "")
               )
       }
-}


apply :
    { width : Float, scale : Float, transitionSpeedInSec : Float }
    -> { from : List (Area msg), to : List (Area msg) }
    -> Html msg
apply ({ width } as options) { from, to } =
    let
        transitions : List ( Float, String )
        transitions =
            [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
            , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
            , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
            ]

        transitionLength : Float
        transitionLength =
            transitions
                |> List.map Tuple.first
                |> List.sum

        animationCss =
            transitions
                |> List.foldl
                    (\( length, css ) ( sum, string ) ->
                        ( sum + length
                        , string
                            ++ (toString <| (sum + length) * 100 / transitionLength)
                            ++ "% {"
                            ++ css
                            ++ "} "
                        )
                    )
                    ( 0, "" )
                |> Tuple.second
                |> (\a -> a ++ ";")

        animationName : String
        animationName =
            "fade"
    in
    div
        [ css
            [ Css.backgroundColor (Css.rgb 0 0 0)
            ]
        ]
        [ Foreign.global
            [ Foreign.selector
                ("@keyframes pixelengine_screen_transition_"
                    ++ animationName
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
                    , Css.opacity <| Css.num 0
                    , Css.property "animation"
                        ("pixelengine_screen_transition_"
                            ++ animationName
                            ++ " "
                            ++ toString transitionLength
                            ++ "s 1"
                        )
                    ]
                ]
                [ Graphics.render options from ]
            ]
        ]
